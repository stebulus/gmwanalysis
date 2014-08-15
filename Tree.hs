-- Binary trees with different types of label at branches and leaves

module Tree where

import Data.Foldable
import Data.Monoid
import Prelude hiding (lookup)

data Tree a b = Branch a (Tree a b) (Tree a b)
              | Leaf b
    deriving (Show, Eq)

instance Functor (Tree a) where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Branch x l r) = Branch x (fmap f l) (fmap f r)

instance Foldable (Tree a) where
    foldMap f (Leaf x) = f x
    foldMap f (Branch _ l r) = foldMap f l <> foldMap f r

instance Monad (Tree a) where
    return x = Leaf x
    (Branch x l r) >>= f = Branch x (l >>= f) (r >>= f)
    (Leaf x) >>= f = f x

leaves :: Tree a b -> [b]
leaves = foldMap (\x -> [x])

branch :: a -> (b,b) -> Tree a b
branch x (l,r) = Branch x (Leaf l) (Leaf r)

splicers :: Tree a b -> [(b -> [Tree a b]) -> [Tree a b]]
splicers (Leaf y) = [($y)]
splicers (Branch x l r) =
    map (map (\t -> Branch x t r) .) (splicers l)
    ++ map (map (\t -> Branch x l t) .) (splicers r)

lookup :: (a->c->Bool) -> c -> Tree a b -> b
lookup _ _ (Leaf y) = y
lookup f x (Branch x' l r) = lookup f x (if (f x' x) then r else l)
