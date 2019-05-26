{-# LANGUAGE FlexibleContexts #-}

module SkiFree where

import Test.QuickCheck
import Test.QuickCheck.Checkers

data S n a = S (n a) a deriving (Eq, Show)

instance ( Functor n, Arbitrary (n a), Arbitrary a ) => Arbitrary (S n a) where
    arbitrary = S <$> arbitrary <*> arbitrary

instance ( Applicative n, Testable (n Property), EqProp a) => EqProp (S n a) where
    (S x y) =-= (S p q) = (property $ (=-=) <$> x <*> p) .&. (y =-= q)

instance Functor n => Functor (S n) where
    fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
    foldr f z (S na a) = f a (foldr f z na)

instance Traversable n => Traversable (S n) where
    --(traverse f na) :: f (n b)
    --          (f a) :: f b
    traverse f (S na a) = (fmap S (traverse f na)) <*> (f a)
