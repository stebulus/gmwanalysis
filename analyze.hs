import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map, elems, assocs)
import qualified Data.Map as M
import Data.Set (Set, member)
import qualified Data.Set as S
import Data.Tuple
import System.Environment
import System.IO
import Text.Printf
import Text.Regex

infix 7 //  -- same as (/)
(//) :: (Integral a, Fractional b) => a -> a -> b
(//) = (/) `on` fromIntegral

picks :: [a] -> [(a,[a])]
picks xs = zip xs (dropped xs)
    where dropped [] = []
          dropped (x:xs) = xs : (map (x:) (dropped xs))

mapsnd :: (a->b) -> [(c,a)] -> [(c,b)]
mapsnd f = map (\(x,y) -> (x, f y))

lastM :: Monad m => [m a] -> m a
lastM (mx:[]) = mx
lastM (mx:mxs) = mx >> lastM mxs
lastM [] = error "empty list"

--
-- Maps whose values are lists
--

conj :: Ord a => a -> b -> Map a [b] -> Map a [b]
conj k v m = M.insert k
                      (v:(fromMaybe [] (M.lookup k m)))
                      m

conjUp :: Ord a => [(a,b)] -> Map a [b]
conjUp = foldl' (\m (k,v) -> conj k v m) M.empty

splitValues :: Ord b => (a->b) -> Map [b] [a] -> Map [b] [a]
splitValues f m = conjUp [ ((f v):k, v) | (k,vs) <- assocs m, v <- vs ]

--
-- Classed: a collection of things classified by feature
--

data Feature a b = Feature { tag :: String , func :: a->b }
instance Show (Feature a b) where
    show f = "f\"" ++ tag f ++ "\""

data Classed a b = Classed { classers :: [Feature a b]
                           , classes :: Map [b] [a]
                           }

nullClassed :: Ord b => [a] -> Classed a b
nullClassed xs = Classed [] (M.insert [] xs M.empty)

splitClasses :: Ord b => Feature a b -> Classed a b -> Classed a b
splitClasses f cl = Classed { classers = f:(classers cl)
                            , classes = splitValues (func f) (classes cl)
                            }

classSize :: Ord b => Classed a b -> [b] -> Int
classSize cl c = length $ fromMaybe [] $ M.lookup c (classes cl)

--
-- Model: a population with a probability distribution which matches
-- a sample, as far as some features are concerned
--

data Model a b = Model { classedPop :: Classed a b
                       , classedSamp :: Classed a b
                       }

nullModel :: Ord b => [a] -> [a] -> Model a b
nullModel = Model `on` nullClassed

refine :: Ord b => Model a b -> Feature a b -> Model a b
refine model f = Model { classedPop = splitClasses f (classedPop model)
                       , classedSamp = splitClasses f (classedSamp model)
                       }

features :: Model a b -> [Feature a b]
features = classers . classedPop

population :: Ord b => Model a b -> [a]
population = concat . elems . classes . classedPop

sample :: Ord b => Model a b -> [a]
sample = concat . elems . classes . classedSamp

classify :: Model a b -> a -> [b]
classify model x = [func f x | f <- features model]

sampleSize :: Ord b => Model a b -> Int
sampleSize = length . sample

weightInClass :: (Ord b, Fractional c) => Model a b -> [b] -> c
weightInClass model cls =
    classSize (classedSamp model) cls
    // ((classSize (classedPop model) cls) * (sampleSize model))

weight :: (Ord b, Fractional c) => Model a b -> a -> c
weight model x = weightInClass model $ classify model x

weights :: (Ord b, Fractional c) => Model a b -> [(a,c)]
weights model = [ (x, wt)
                | (cls, xs) <- assocs $ classes $ classedPop model
                , let wt = weightInClass model cls
                , x <- xs ]

showWeight :: (String, Float) -> String
showWeight (word, weight) = printf "%s %.8f" word (10000*weight)

hPutWeights :: Ord b => Handle -> Model String b -> IO ()
hPutWeights hout model = do
    mapM_ (hPutStrLn hout) $ map showWeight $ sort $ weights model

--
-- Refining a Model to increase the likelihood of its sample
--

sampleLogLikelihood :: (Eq b, Ord b, Floating c) => Model a b -> c
sampleLogLikelihood model =
    sum [ fromIntegral (length sampcls) * log (weightInClass model cls)
        | (cls,sampcls) <- assocs $ classes $ classedSamp model ]

refineBest :: (Eq b, Ord b) =>
    (Model a b,[Feature a b]) -> (Model a b,[Feature a b])
refineBest (model, features) =
    maximumBy (compare `on` (sampleLogLikelihood.fst))
              [(refine model f, fs) | (f,fs) <- picks features]

refinements :: (Eq b, Ord b) => Model a b -> [Feature a b] -> [Model a b]
refinements model features =
    map fst
    $ takeWhile (not . null . snd)
    $ iterate refineBest (model, features)

--
-- Some features
--

fromSet :: Ord a => String -> Set a -> Feature a Bool
fromSet tag set = Feature tag (`member` set)

fromList :: Ord a => [(String,a)] -> [Feature a Bool]
fromList xs = [ fromSet k $ S.fromList vs
              | (k,vs) <- assocs $ conjUp xs ]

logFreqFeatures :: (RealFrac a, Floating a) =>
    [(String, a)] -> [Feature String Bool]
logFreqFeatures lst =
    fromList $ map swap $ mapsnd ((++"logfreq") . show . floor . log) lst

parseFreqData :: [String] -> [(String, Integer)]
parseFreqData xs = [ let [word, intstr] = words x
                     in (word, read intstr)
                   | x<-xs ]

regexFeatures :: [String] -> [(String,String)] -> [Feature String Bool]
regexFeatures regexes texts =
    fromList [ (rename, word)
             | (word,page) <- texts, (rename,re) <- res,
               isJust $ matchRegex re page ]
    where res = zip regexes $ map mkRegex regexes

--
-- Testing performance on a smallish example from twl and words
--

main = do
    n <- fmap (read . (!!0)) getArgs :: IO Int
    allwords <- fmap ((take 40000) . lines) $ readFile "twl"
    let allwordsSet = S.fromList allwords
    chosenwords <-
        fmap (filter (`member` allwordsSet))
        $ fmap (map ((!!2) . words))
        $ fmap lines
        $ readFile "words"
    freqdata <- fmap (parseFreqData . lines) $ readFile "freq"
    wiktpatterns <- fmap lines $ readFile "wikt/macro-patterns"
    wiktreduced <- fmap (map (break (==' ')))
                       $ fmap lines
                       $ readFile "wikt/reduced"
    let feats = logFreqFeatures (mapsnd fromInteger freqdata)
                    ++ regexFeatures wiktpatterns wiktreduced
    bestmodel <- lastM $ take n
        $ map (\model -> do
            print (features model, sampleLogLikelihood model)
            return model)
        $ refinements (nullModel allwords chosenwords) feats
    mapM_ print $ take 25 $ sortBy (flip (compare `on` snd)) $ weights bestmodel
