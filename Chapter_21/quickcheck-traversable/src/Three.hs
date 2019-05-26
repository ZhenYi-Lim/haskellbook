module Three
      (Three (Three)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three a b c = Three a b c deriving (Eq, Show)
    
instance Functor (Three a b) where
    fmap f (Three y z x) = Three y z (f x)

instance Foldable (Three a b) where
    foldMap f (Three _ _ x) = f x

instance Traversable (Three a b) where
    traverse f (Three y z x) = fmap (Three y z) (f x)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Three x y z)

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (=-=) = eq