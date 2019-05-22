module Lst
    ( Lst (Nil, Cons)
    , append
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Lst a = Nil | Cons a (Lst a) deriving (Eq, Show)

append :: Lst a -> Lst a -> Lst a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> Lst a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: Lst (Lst a) -> Lst a
concat' = fold append Nil

-- write this one in terms
-- of concat' and fmap
flatMap :: (a -> Lst b) -> Lst a -> Lst b
flatMap f as = concat' (fmap f as)

instance Arbitrary a => Arbitrary (Lst a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil), (2, return (Cons x y)) ]

instance Eq a => EqProp (Lst a) where
    (=-=) = eq

instance Functor Lst where
    fmap _ Nil = Nil
    fmap f (Cons h t) = (Cons (f h) (fmap f t))

instance Applicative Lst where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    (Cons fh ft) <*> ds = append (fmap fh ds) (ft <*> ds)

instance Monad Lst where
    return = pure
    Nil >>= f = Nil
    xs >>= f = concat' (fmap f xs)