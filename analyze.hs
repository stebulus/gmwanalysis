{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

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
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
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

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x =
    map fromJust
    $ takeWhile isJust
    $ iterate (>>= f) (Just x)

withValue :: Ord a => Map a b -> [a] -> [(a,b)]
withValue m = map (applysnd fromJust)
    . filter (isJust . snd)
    . map (\x -> (x, M.lookup x m))

--
-- Statistics
--

data MeanAndVarAcc = MeanAndVarAcc {-# UNPACK #-} !Int
                                   {-# UNPACK #-} !Float
                                   {-# UNPACK #-} !Float

meanAndVar :: [Float] -> (Float,Float)
meanAndVar xs = (mean, meansq-mean*mean)
    where MeanAndVarAcc ilen sum sumsq =
              foldl' (\ (MeanAndVarAcc len sum sumsq) x ->
                        MeanAndVarAcc (len+1) (sum+x) (sumsq+x*x))
                     (MeanAndVarAcc 0 0 0)
                     xs
          len = fromIntegral ilen
          mean = sum/len
          meansq = sumsq/len

normal :: Floating a => a -> a -> a -> a
normal mean var = (\x -> exp(-(x-mean)^2/(2*var)) / (sqrt (2*pi*var)))

fitNormal :: [Float] -> Float -> Float
fitNormal xs = normal mean var where (mean,var) = meanAndVar xs

--
-- Model: a population with a probability distribution which matches
-- a sample, as far as some features are concerned
--

data Feature a b = Feature { tag :: Text , func :: a->b }
instance Show (Feature a b) where
    show f = "f\"" ++ unpack (tag f) ++ "\""

type Class a = ([a],[a],[Feature a Bool])

type ClassWeight a c = [a] -> [a] -> a -> c

sizeClsWt :: Fractional c => Int -> ClassWeight a c
sizeClsWt samplesz pop samp _ =
    length samp // (length pop * samplesz)

fitNormalClsWt :: (a->Float) -> ClassWeight a Float
fitNormalClsWt f pop samp x = sampwt x / popwt x
    where popwt = fitNormal (map f pop) . f
          sampwt = fitNormal (map f samp) . f

compound :: Num c => ClassWeight a c -> ClassWeight a c -> ClassWeight a c
compound f g pop samp x = (f pop samp x) * (g pop samp x)

classWt :: Model a -> (a->Float) -> ClassWeight a Float
classWt model logfreq =
    compound (sizeClsWt $ sampleSize model)
             (fitNormalClsWt logfreq)

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

weight :: Model a -> (a->Float) -> a -> Float
weight model logfreq x = classWt model logfreq pop samp x
    where (pop,samp,_) = featureLookup x model

weights :: Model a -> (a->Float) -> [(a,Float)]
weights model logfreq = fold $ fmap popWt model
    where popWt (pop,samp,_) = let clswt = classWt model logfreq pop samp
                               in zip pop $ map clswt pop

showWeight :: (Text, Float) -> String
showWeight (word, weight) = printf "%s %.8f" (unpack word) (10000*weight)

hPutWeights :: Handle -> Model (Text,a) -> ((Text,a)->Float) -> IO ()
hPutWeights hout model logfreq = do
    mapM_ (hPutStrLn hout)
    $ map showWeight
    $ sort
    $ map (\ ((word,_),wt) -> (word,wt))
    $ weights model logfreq

--
-- Refining a Model to increase the likelihood of its sample
--

sampleLogLikelihood :: Model a -> (a->Float) -> Float
sampleLogLikelihood model logfreq =
    sum $ do
        (pop,samp,_) <- Tree.toList model
        let wt = classWt model logfreq pop samp
        map (log . wt) samp

logLikelihood :: Model a -> (a->Float) -> [a] -> Float
logLikelihood model logfreq xs = sum $ map (log.(weight model logfreq)) xs

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

fromSet :: Ord a => Text -> Set a -> Feature a Bool
fromSet tag set = Feature tag (`member` set)

hLineSet :: Handle -> IO (Set Text)
hLineSet h = do
    txt <- TIO.hGetContents h
    return $ foldl' (flip S.insert) S.empty $ T.lines txt
lineSet :: FilePath -> IO (Set Text)
lineSet path = withFile path ReadMode hLineSet

--
-- Input of word-float data
--

hWordNumMap :: Handle -> IO (Map Text Float)
hWordNumMap h = do
    txt <- TIO.hGetContents h
    return $ foldl' (\ map [k,v] -> M.insert k (read $ unpack v) map)
                    M.empty
           $ map T.words
           $ T.lines txt
wordNumMap :: FilePath -> IO (Map Text Float)
wordNumMap path = withFile path ReadMode hWordNumMap

linesWith :: Map Text b -> FilePath -> IO [(Text,b)]
linesWith m path = withFile path ReadMode
    (\h -> do
        txt <- TIO.hGetContents h
        return $ withValue m $ T.lines txt)

--
-- Train and emit a model for the files specified on the command line
--

main = do
    allwordsfile : chosenwordsfile : logfreqfile : setfiles <- getArgs

    logfreq <- wordNumMap logfreqfile
    allwords <- fmap (take 2000) $ linesWith logfreq allwordsfile
    chosenwords <- fmap ( S.toList
                        . (`S.intersection` (S.fromList allwords))
                        . S.fromList
                        )
        $ linesWith logfreq chosenwordsfile
    feats <- forM setfiles
        (\path -> fmap (fromSet (pack path) . S.fromList)
                       $ linesWith logfreq path)
    let logfreqf = snd

    let (heldback,trainingset) =
            partitionByIndex (\n -> n `mod` 3 == 0) chosenwords
    bestmodel <- lastM
        $ map (\ (model, xval) -> do
            hPutStrLn stderr $ show
                ( fmap (\ (pop,samp,feats) -> (length pop, length samp, length feats)) model
                , sampleLogLikelihood model logfreqf
                , xval
                )
            return model)
        $ increasingPrefix (compare `on` snd)
        $ map (\model -> (model, logLikelihood model logfreqf heldback))
        $ bestRefinements (flip sampleLogLikelihood logfreqf)
        $ nullModel allwords trainingset feats
    hPutWeights stdout bestmodel logfreqf
