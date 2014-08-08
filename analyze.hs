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

data Model a b = Model { features :: [Feature a b]
                       , population :: [a]
                       , sample :: [a]
                       }

nullModel :: [a] -> [a] -> Model a b
nullModel = Model []

classify :: Model a b -> a -> [b]
classify model x = [func f x | f <- features model]

refine :: Model a b -> Feature a b -> Model a b
refine model f = model { features = f:(features model) }

weight :: (Eq b, Fractional c) => Model a b -> a -> c
weight model x =
    (countClass samp) // ((countClass pop) * (length samp))
    where cls = classify model x
          countClass xs = length $ filter (== cls) $ map (classify model) xs
          pop = population model
          samp = sample model

sampleLikelihood :: (Eq b, Floating c) => Model a b -> c
sampleLikelihood model =
    sum $ map log $ map (weight model) $ sample model

refineBest :: Eq b => (Model a b,[Feature a b]) -> (Model a b,[Feature a b])
refineBest (model, features) =
    maximumBy (compare `on` (sampleLikelihood.fst))
              [(refine model f, fs) | (f,fs) <- picks features]

refinements :: Eq b => Model a b -> [Feature a b] -> [Model a b]
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
