import Control.Applicative
import Data.Function
import Data.List
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

weight :: (Eq b, Fractional c) => [a] -> [a] -> Feature a b -> a -> c
weight population sample classify x =
    (countClass sample) // ((countClass population) * (length sample))
    where cls = func classify $ x
          countClass xs = length $ filter (== cls) $ map (func classify) xs

sampleLikelihood :: (Eq b, Floating c) => [a] -> [a] -> Feature a b -> c
sampleLikelihood population sample feature =
    sum $ map log $ map (weight population sample feature) sample

compositeFeature :: [Feature a b] -> Feature a [b]
compositeFeature fs = Feature (intercalate " : " (map tag fs))
                              (\x -> map (($ x) . func) fs)

refine :: Eq b => [a] -> [a]
    -> ([Feature a b],[Feature a b]) -> ([Feature a b],[Feature a b])
refine population sample (model,features) =
    maximumBy (compare `on` (quality.fst))
              [(f:model,fs) | (f,fs) <- picks features]
    where quality model = sampleLikelihood population sample $ compositeFeature model

refinements :: Eq b => [a] -> [a] -> [Feature a b] -> [[Feature a b]]
refinements population sample features =
    map fst
    $ takeWhile (not . null . snd)
    $ iterate (refine population sample) ([], features)

main = do
    n <- fmap (read . (!!0)) getArgs :: IO Int
    allwords <- fmap ((take 5000) . lines) $ readFile "twl"
    let allwordsSet = S.fromList allwords
    chosenwords <-
        fmap (filter (`S.member` allwordsSet))
        $ fmap (map ((!!2) . words))
        $ fmap lines
        $ readFile "words"
    let features = [Feature [letter] (\word -> last word == letter)
                   | letter<-['a'..'z']]
    mapM_ print $ take n $
        [(fs, sampleLikelihood allwords chosenwords $ compositeFeature fs)
        | fs<-refinements allwords chosenwords features]
