module Lst
      (Lst (Nil, Cons)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Lst a = Nil | Cons a (Lst a) deriving (Eq, Show)
    
instance Functor Lst where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable Lst where
    foldMap f (Cons x xs) = (f x) <> (foldMap f xs)
    foldMap _ Nil = mempty

instance Traversable Lst where
    traverse _ Nil = pure Nil
    traverse f (Cons x xs) = (fmap Cons (f x)) <*> (traverse f xs)
    --traverse f (Cons x xs) = Cons <$> (f x) <*> (traverse f xs)

instance Arbitrary a => Arbitrary (Lst a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil), (2, return (Cons x y)) ]

instance Eq a => EqProp (Lst a) where
    (=-=) = eq