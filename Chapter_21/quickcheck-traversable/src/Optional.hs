module Optional
      (Optional (Nada, Yep)
      ) where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Optional a = Nada | Yep a deriving (Eq, Show)
    
instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep x) = Yep (f x)

instance Foldable Optional where
    foldMap f (Yep x) = f x
    foldMap _ Nada = mempty

instance Traversable Optional where
    traverse _ Nada = pure Nada
    traverse f (Yep x) = fmap Yep (f x)

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        x <- arbitrary
        frequency [ (1, return Nada), (1, return (Yep x)) ]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq