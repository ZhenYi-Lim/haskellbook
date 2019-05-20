module ZipLst'
    ( ZipLst' (ZipLst')
    --, Lst' (Nil', Cons')
    )where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lst

--tried with a custom version Lst', but does not satisfy the applicative laws
{- data Lst' a = Nil' | Cons' a (Lst' a) deriving (Eq, Show)

instance Functor Lst' where
    fmap _ Nil' = Nil'
    fmap f (Cons' h t) = (Cons' (f h) (fmap f t))

instance Applicative Lst' where
    pure x = Cons' x Nil'
    Nil' <*> _ = Nil'
    _ <*> Nil' = Nil'
    (Cons' fh ft) <*> (Cons' dh dt) = Cons' (fh dh) (ft <*> dt)

instance Arbitrary a => Arbitrary (Lst' a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        frequency [ (1, return Nil'), (2, return (Cons' x y)) ]

instance Eq a => EqProp (Lst' a) where
    (=-=) = eq

take' :: Int -> Lst' a -> Lst' a
take' 0 _ = Nil'
take' _ Nil' = Nil'
take' n (Cons' h t) = Cons' h (take' (n-1) t)

newtype ZipLst' a = ZipLst' (Lst' a) deriving (Eq, Show)

instance Eq a => EqProp (ZipLst' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipLst' l) = xs
                    in take' 3000 l
              ys' = let (ZipLst' l) = ys
                    in take' 3000 l

instance Semigroup ZipLst' where
    ZipLst' x <> ZipLst' y = ZipLst' (append x y)

instance Functor ZipLst' where
    fmap f (ZipLst' xs) = ZipLst' $ fmap f xs

instance Applicative ZipLst' where
    pure x = ZipLst' (Cons' x Nil')
    _ <*> (ZipLst' Nil') = ZipLst' Nil'
    (ZipLst' Nil') <*> _ = ZipLst' Nil'
    (ZipLst' fs) <*> (ZipLst' ds) = ZipLst' (fs <*> ds) -}

take' :: Int -> Lst a -> Lst a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons h t) = Cons h (take' (n-1) t)

newtype ZipLst' a = ZipLst' (Lst a) deriving (Eq, Show)

instance Eq a => EqProp (ZipLst' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipLst' l) = xs
                    in take' 3000 l
              ys' = let (ZipLst' l) = ys
                    in take' 3000 l

instance Functor ZipLst' where
    fmap f (ZipLst' xs) = ZipLst' $ fmap f xs

instance Applicative ZipLst' where
    pure x = ZipLst' (Cons x Nil)
    _ <*> (ZipLst' Nil) = ZipLst' Nil
    (ZipLst' Nil) <*> _ = ZipLst' Nil
     --producing the results shown in the example results in an invalid applicative instance? Definitely doesn't satisfy the identity law.
    (ZipLst' (Cons fh ft)) <*> (ZipLst' (Cons dh dt)) = ZipLst'.append (ZipLst' (Cons (fh dh) Nil)) ((ZipLst' ft) <*> (ZipLst' dt))

instance Arbitrary a => Arbitrary (ZipLst' a) where
    arbitrary = do
        x <- arbitrary
        return (ZipLst' x)

append :: ZipLst' a -> ZipLst' a -> ZipLst' a
append (ZipLst' x) (ZipLst' y) = ZipLst' (Lst.append x y)