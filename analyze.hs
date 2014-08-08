import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map, elems, assocs)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment

infix 7 //  -- same as (/)
(//) :: (Integral a, Fractional b) => a -> a -> b
(//) = (/) `on` fromIntegral

picks :: [a] -> [(a,[a])]
picks xs = zip xs (dropped xs)
    where dropped [] = []
          dropped (x:xs) = xs : (map (x:) (dropped xs))

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

--
-- Refining a Model to increase the likelihood of its sample
--

sampleLikelihood :: (Eq b, Ord b, Floating c) => Model a b -> c
sampleLikelihood model =
    sum [ fromIntegral (length sampcls) * log (weightInClass model cls)
        | (cls,sampcls) <- assocs $ classes $ classedSamp model ]

refineBest :: (Eq b, Ord b) =>
    (Model a b,[Feature a b]) -> (Model a b,[Feature a b])
refineBest (model, features) =
    maximumBy (compare `on` (sampleLikelihood.fst))
              [(refine model f, fs) | (f,fs) <- picks features]

refinements :: (Eq b, Ord b) => Model a b -> [Feature a b] -> [Model a b]
refinements model features =
    map fst
    $ takeWhile (not . null . snd)
    $ iterate refineBest (model, features)

--
-- Testing performance on a smallish example from twl and words
--

main = do
    n <- fmap (read . (!!0)) getArgs :: IO Int
    allwords <- fmap ((take 40000) . lines) $ readFile "twl"
    let allwordsSet = S.fromList allwords
    chosenwords <-
        fmap (filter (`S.member` allwordsSet))
        $ fmap (map ((!!2) . words))
        $ fmap lines
        $ readFile "words"
    let feats = [Feature [letter] (\word -> last word == letter)
                | letter<-['a'..'z']]
    mapM_ print $ take n $
        [(features model, sampleLikelihood model)
        | model<-refinements (nullModel allwords chosenwords) feats]
