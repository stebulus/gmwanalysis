module Main where

import Prelude hiding (sum)
import Control.Monad
import Data.Foldable (foldMap, fold, sum)
import Data.Function
import Data.List (partition, sort, maximumBy, foldl')
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe
import Data.Set (Set, member)
import qualified Data.Set as S
import Data.Tuple
import System.Environment
import System.IO
import Text.Printf
import Tree

--
-- Generally handy functions
--

infix 7 //  -- same as (/)
(//) :: (Integral a, Fractional b) => a -> a -> b
(//) = (/) `on` fromIntegral

picks :: [a] -> [(a,[a])]
picks xs = zip xs (dropped xs)
    where dropped [] = []
          dropped (x:xs) = xs : (map (x:) (dropped xs))

applyboth :: (a->b) -> (a,a) -> (b,b)
applyboth f (x,y) = (f x, f y)
applysnd :: (a->b) -> (c,a) -> (c,b)
applysnd f (x,y) = (x,f y)

lastM :: Monad m => [m a] -> m a
lastM (mx:[]) = mx
lastM (mx:mxs) = mx >> lastM mxs
lastM [] = error "empty list"

partitionByIndex :: (Enum b, Num b) => (b->Bool) -> [a] -> ([a],[a])
partitionByIndex f xs =
    (yes,no)
    where ((_,yes),(_,no)) =
            applyboth unzip
            $ partition (f.fst)
            $ zip [0..] xs

increasingPrefix :: (a->a->Ordering) -> [a] -> [a]
increasingPrefix cmp (x:y:rest) =
    x:(case cmp x y of
        LT -> increasingPrefix cmp (y:rest)
        EQ -> increasingPrefix cmp (y:rest)
        GT -> [])
increasingPrefix _ xs = xs

foldLines :: (a -> String -> a) -> a -> Handle -> IO a
foldLines f init h = seq init $ do    -- the seq here halves memory usage
    eof <- hIsEOF h
    if eof
      then return init
      else do
        line <- hGetLine h
        foldLines f (f init line) h

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
    map fromJust
    $ takeWhile isJust
    $ iterate (>>= f) (Just x)

--
-- Statistics
--

meanAndVar :: Fractional a => [a] -> (a,a)
meanAndVar xs = (mean, meansq-mean*mean)
    where (ilen,sum,sumsq) =
              foldl' (\ (len,sum,sumsq) x -> (len+1, sum + x, sumsq + x*x))
                     (0,0,0)
                     xs
          len = fromIntegral ilen
          mean = sum/len
          meansq = sumsq/len

normal :: Floating a => a -> a -> a -> a
normal mean var = (\x -> exp(-(x-mean)^2/(2*var)) / (sqrt (2*pi*var)))

fitNormal :: Floating a => [a] -> a -> a
fitNormal xs = normal mean var where (mean,var) = meanAndVar xs

--
-- Model: a population with a probability distribution which matches
-- a sample, as far as some features are concerned
--

data Feature a b = Feature { tag :: String , func :: a->b }
instance Show (Feature a b) where
    show f = "f\"" ++ tag f ++ "\""

type Class a = ([a],[a],[Feature a Bool])

type Model a = Tree (Feature a Bool) (Class a)

featureLookup :: a -> Tree (Feature a Bool) b -> b
featureLookup = Tree.lookup (\feat x -> (func feat) x)

nullModel :: [a] -> [a] -> [Feature a Bool] -> Model a
nullModel xs ys fs = Leaf (xs,ys,fs)

population :: Model a -> [a]
population = foldMap (\ (xs,_,_) -> xs)

sample :: Model a -> [a]
sample = foldMap (\ (_,ys,_) -> ys)

sampleSize :: Model a -> Int
sampleSize = length . sample

weight :: Fractional c => Model a -> a -> c
weight model x =
    length samp // (length pop * sampleSize model)
    where (pop,samp,_) = featureLookup x model

addWeights :: Fractional c => Model a -> Tree (Feature a Bool) (Class a,c)
addWeights model = fmap addWt model
    where addWt cl@(pop,samp,_) = (cl, length samp // (length pop * totsamplesz))
          totsamplesz = sampleSize model

weights :: Fractional c => Model a -> [(a,c)]
weights model = fold $ fmap popWt $ addWeights model
    where popWt ((pop,_,_),wt) = zip pop (repeat wt)

showWeight :: (String, Float) -> String
showWeight (word, weight) = printf "%s %.8f" word (10000*weight)

hPutWeights :: Handle -> Model String -> IO ()
hPutWeights hout model = do
    mapM_ (hPutStrLn hout) $ map showWeight $ sort $ weights model

--
-- Refining a Model to increase the likelihood of its sample
--

sampleLogLikelihood :: Floating c => Model a -> c
sampleLogLikelihood model =
    sum
    $ fmap (\ ((_,samp,_),wt) -> let len = length samp
                               in if len == 0
                                    then 0
                                    else fromIntegral len * log wt)
    $ addWeights model

logLikelihood :: Floating c => Model a -> [a] -> c
logLikelihood model xs = sum $ map (log.(weight model)) xs

classRefinements :: Class a -> [Model a]
classRefinements (pop, samp, features) =
    [ branch feat ((popt,sampt,feats),(popf,sampf,feats))
    | (feat, feats) <- picks features
    , let (popt,popf) = split feat pop
    , let (sampt,sampf) = split feat samp
    ]
    where split feat = swap . partition (func feat)

refinements :: Model a -> [Model a]
refinements model = splicers model >>= ($ classRefinements)

refineBest :: Ord c => (Model a -> c) -> Model a -> Maybe (Model a)
refineBest quality model = case refinements model of
        [] -> Nothing
        xs -> Just $ maximumBy (compare `on` quality) xs

bestRefinements :: Ord c => (Model a -> c) -> Model a -> [Model a]
bestRefinements quality model = iterateMaybe (refineBest quality) model

--
-- Construction of features
--

fromSet :: Ord a => String -> Set a -> Feature a Bool
fromSet tag set = Feature tag (`member` set)

hLineSet :: Handle -> IO (Set String)
hLineSet = foldLines (flip S.insert) S.empty
lineSet :: FilePath -> IO (Set String)
lineSet path = withFile path ReadMode hLineSet

--
-- Input of word-float data
--

hWordNumMap :: Handle -> IO (Map String Float)
hWordNumMap = foldLines parse M.empty
    where parse map line = let [a,b] = words line
                           in M.insert a (read b) map
wordNumMap :: FilePath -> IO (Map String Float)
wordNumMap path = withFile path ReadMode hWordNumMap

--
-- Train and emit a model for the files specified on the command line
--

main = do
    allwordsfile : chosenwordsfile : logfreqfile : setfiles <- getArgs

    allwordsSet <- lineSet allwordsfile
    chosenwords <- fmap (`S.intersection` allwordsSet)
        $ lineSet chosenwordsfile
    feats <- forM setfiles
        (\path -> fmap (fromSet path) (lineSet path))

    let (heldback,trainingset) =
            partitionByIndex (\n -> n `mod` 3 == 0) (S.toList chosenwords)
    bestmodel <- lastM
        $ map (\ (model, xval) -> do
            hPutStrLn stderr $ show
                ( fmap (\ (pop,samp,feats) -> (length pop, length samp, length feats)) model
                , sampleLogLikelihood model
                , xval
                )
            return model)
        $ increasingPrefix (compare `on` snd)
        $ map (\model -> (model, logLikelihood model heldback))
        $ bestRefinements sampleLogLikelihood
        $ nullModel (S.toList allwordsSet) trainingset feats
    hPutWeights stdout bestmodel
