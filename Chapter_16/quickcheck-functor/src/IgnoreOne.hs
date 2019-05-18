module IgnoreOne
     ( IgnoreOne (IgnoreSomething)
     ) where

import Test.QuickCheck

data IgnoreOne f g a b = IgnoreSomething (f a) (g b) deriving (Eq, Show)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething fa gb) = IgnoreSomething fa (fmap f gb)