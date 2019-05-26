module Big
      (Big (Big)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Big a b = Big a b b deriving (Eq, Show)
    
instance Functor (Big a) where
    fmap f (Big y x1 x2) = Big y (f x1) (f x2)

instance Foldable (Big a) where
    foldMap f (Big _ x1 x2) = (f x1) <> (f x2)

instance Traversable (Big a) where
    traverse f (Big y x1 x2) = (fmap (Big y) (f x1)) <*> (f x2)
    --traverse f (Big y x1 x2) = (Big y) <$> (f x1) <*> (f x2)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return (Big x y z)

instance (Eq a, Eq b) => EqProp (Big a b) where
    (=-=) = eq