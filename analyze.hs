import Control.Applicative
import Data.Function
import Data.List
import Data.Map (Map, elems)
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

data Feature a b = Feature { tag :: String , func :: a->b }
instance Show (Feature a b) where
    show f = "f\"" ++ tag f ++ "\""

data Model a b = Model { features :: [Feature a b]
                       , classedPop :: Map [b] [a]
                       , classedSamp :: Map [b] [a]
                       }

population :: Ord b => Model a b -> [a]
population = concat . elems . classedPop

sample :: Ord b => Model a b -> [a]
sample = concat . elems . classedSamp

nullModel :: Ord b => [a] -> [a] -> Model a b
nullModel = Model [] `on` (\v -> M.insert [] v M.empty)

classify :: Model a b -> a -> [b]
classify model x = [func f x | f <- features model]

sampleSize :: Ord b => Model a b -> Int
sampleSize = length . sample

classSizePop :: (Eq b, Ord b) => Model a b -> [b] -> Int
classSizePop model cls =
    length $ filter (== cls) $ map (classify model) $ population model

classSizeSamp :: (Eq b, Ord b) => Model a b -> [b] -> Int
classSizeSamp model cls =
    length $ filter (== cls) $ map (classify model) $ sample model

refine :: Model a b -> Feature a b -> Model a b
refine model f = model { features = f:(features model) }

weight :: (Eq b, Ord b, Fractional c) => Model a b -> a -> c
weight model x =
    classSizeSamp model cls // ((classSizePop model cls) * (sampleSize model))
    where cls = classify model x

sampleLikelihood :: (Eq b, Ord b, Floating c) => Model a b -> c
sampleLikelihood model =
    sum $ map log $ map (weight model) $ sample model

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

main = do
    n <- fmap (read . (!!0)) getArgs :: IO Int
    allwords <- fmap ((take 5000) . lines) $ readFile "twl"
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
