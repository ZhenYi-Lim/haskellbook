module Identity
      (Identity (Identity)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
    foldr f z (Identity x) = f x z

instance Traversable Identity where
    traverse f (Identity x) = fmap Identity (f x)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq