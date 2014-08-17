{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (sum)
import Control.Monad
import Data.Foldable (fold, sum)
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

--
-- Model: a population with a probability distribution which matches
-- a sample, as far as some features are concerned
--

data Feature a b = Feature { tag :: Text , func :: a->b }
instance Show (Feature a b) where
    show f = "f\"" ++ unpack (tag f) ++ "\""

data ClassWeightParams = ClassWeightParams { totalWeight :: Float
                                           , populationMean :: Float
                                           , populationVar :: Float
                                           , sampleMean :: Float
                                           , sampleVar :: Float
                                           }

weightFromParams :: ClassWeightParams -> Float -> Float
weightFromParams params logfreq =
    totalWeight params * sampnwt logfreq / popnwt logfreq
    where popnwt = normal (populationMean params) (populationVar params)
          sampnwt = normal (sampleMean params) (sampleVar params)

makeParams :: Int -> (a->Float) -> [a] -> [a] -> ClassWeightParams
makeParams samplesz f pop samp =
    ClassWeightParams { totalWeight = length samp // (length pop * samplesz)
                      , populationMean = popmean
                      , populationVar = popvar
                      , sampleMean = sampmean
                      , sampleVar = sampvar
                      }
    where (popmean,popvar) = meanAndVar (map f pop)
          (sampmean,sampvar) = meanAndVar (map f samp)

data Class a = Class { population :: [a]
                     , sample :: [a]
                     , totalSampleSize :: Int
                     , logfreqf :: a->Float
                     , unusedFeatures :: [Feature a Bool]
                     , clswtParams :: ClassWeightParams
                     , sampleLL :: Float
                     }
makeClass :: [a] -> [a] -> [Feature a Bool] -> Int -> (a->Float) -> Class a
makeClass pop samp feats samplesz logfreqf =
    Class pop samp samplesz logfreqf feats params sampll
    where params = makeParams samplesz logfreqf pop samp
          sampll = sum $ map (log . weightFromParams params . logfreqf) samp

type Model a = Tree (Feature a Bool) (Class a)

featureLookup :: a -> Tree (Feature a Bool) b -> b
featureLookup = Tree.lookup (\feat x -> (func feat) x)

nullModel :: [a] -> [a] -> [Feature a Bool] -> (a->Float) -> Model a
nullModel xs ys fs logfreq = Leaf
    $ makeClass xs ys fs (length ys) logfreq

weight :: Model a -> a -> Float
weight model x =
    weightFromParams (clswtParams cls) (logfreqf cls x)
    where cls = featureLookup x model

weights :: Model a -> [(a,Float)]
weights model = fold $ fmap popWt model
    where popWt cls =
            let wt = \x -> weightFromParams (clswtParams cls) (logfreqf cls x)
            in zip (population cls) $ map wt (population cls)

showWeight :: (Text, Float) -> String
showWeight (word, weight) = printf "%s %.8f" (unpack word) (10000*weight)

hPutWeights :: Handle -> Model (Text,a) -> IO ()
hPutWeights hout model = do
    mapM_ (hPutStrLn hout)
    $ map showWeight
    $ sort
    $ map (\ ((word,_),wt) -> (word,wt))
    $ weights model

--
-- Refining a Model to increase the likelihood of its sample
--

sampleLogLikelihood :: Model a -> Float
sampleLogLikelihood model = sum $ map sampleLL $ Tree.toList model

logLikelihood :: Model a -> [a] -> Float
logLikelihood model xs = sum $ map (log.(weight model)) xs

classRefinements :: Class a -> [Model a]
classRefinements cls =
    [ branch feat ( makeClass popt sampt feats (totalSampleSize cls) (logfreqf cls)
                  , makeClass popf sampf feats (totalSampleSize cls) (logfreqf cls)
                  )
    | (feat, feats) <- picks (unusedFeatures cls)
    , let (popt,popf) = split feat (population cls)
    , let (sampt,sampf) = split feat (sample cls)
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
                ( fmap (\ cls ->
                           ( length (population cls)
                           , length (sample cls)
                           , length (unusedFeatures cls)
                           )
                       )
                       model
                , sampleLogLikelihood model
                , xval
                )
            return model)
        $ increasingPrefix (compare `on` snd)
        $ map (\model -> (model, logLikelihood model heldback))
        $ bestRefinements sampleLogLikelihood
        $ nullModel allwords trainingset feats logfreqf
    hPutWeights stdout bestmodel
