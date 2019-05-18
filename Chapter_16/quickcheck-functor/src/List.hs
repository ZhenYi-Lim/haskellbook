module List
     ( List (Nil, Cons)
     ) where

import Test.QuickCheck

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x lx) = Cons (f x) (fmap f lx)