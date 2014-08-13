import Control.Applicative
import Control.Monad
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

logLikelihood :: (Ord b, Floating c) => Model a b -> [a] -> c
logLikelihood model xs = sum $ map (log.(weight model)) xs

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

hLineSet :: Handle -> IO (Set String)
hLineSet = foldLines (flip S.insert) S.empty
lineSet :: FilePath -> IO (Set String)
lineSet path = withFile path ReadMode hLineSet

fromList :: Ord a => [(String,a)] -> [Feature a Bool]
fromList xs = [ fromSet k $ S.fromList vs
              | (k,vs) <- assocs $ conjUp xs ]

--
-- Testing performance on a smallish example from twl and words
--

main = do
    args <- getArgs
    let chosenwordsfile = head args
    let setfiles = tail args

    allwordsSet <- lineSet "twl"
    chosenwords <- fmap (`S.intersection` allwordsSet)
        $ lineSet chosenwordsfile
    feats <- forM setfiles
        (\path -> fmap (fromSet path) (lineSet path))

    let (heldback,trainingset) =
            partitionByIndex (\n -> n `mod` 3 == 0) (S.toList chosenwords)
    bestmodel <- lastM
        $ map (\ (model, xval) -> do
            hPutStrLn stderr $ show
                ( features model
                , sampleLogLikelihood model
                , xval
                )
            return model)
        $ increasingPrefix (compare `on` snd)
        $ map (\model -> (model, logLikelihood model heldback))
        $ refinements (nullModel (S.toList allwordsSet) trainingset) feats
    hPutWeights stdout bestmodel
