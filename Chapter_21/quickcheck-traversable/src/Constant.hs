module Constant
      (Constant (Constant)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Constant a b = Constant { getConstant :: a } deriving (Eq, Show)
    
instance Functor (Constant a) where
    fmap _ (Constant x) = Constant x

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse _ (Constant x) = pure $ Constant x

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq