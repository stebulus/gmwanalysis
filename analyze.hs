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

data Model a b = Model { features :: [Feature a b] }

classify :: Model a b -> a -> [b]
classify model x = [func f x | f <- features model]

refine :: Model a b -> Feature a b -> Model a b
refine model f = Model (f:(features model))

weight :: (Eq b, Fractional c) => [a] -> [a] -> Model a b -> a -> c
weight population sample model x =
    (countClass sample) // ((countClass population) * (length sample))
    where cls = classify model x
          countClass xs = length $ filter (== cls) $ map (classify model) xs

sampleLikelihood :: (Eq b, Floating c) => [a] -> [a] -> Model a b -> c
sampleLikelihood population sample model =
    sum $ map log $ map (weight population sample model) sample

refineBest :: Eq b => [a] -> [a]
    -> (Model a b,[Feature a b]) -> (Model a b,[Feature a b])
refineBest population sample (model, features) =
    maximumBy (compare `on` (quality.fst))
              [(refine model f, fs) | (f,fs) <- picks features]
    where quality model = sampleLikelihood population sample model

refinements :: Eq b => [a] -> [a] -> [Feature a b] -> [Model a b]
refinements population sample features =
    map fst
    $ takeWhile (not . null . snd)
    $ iterate (refineBest population sample) (Model [], features)

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
        [(features model, sampleLikelihood allwords chosenwords model)
        | model<-refinements allwords chosenwords feats]
