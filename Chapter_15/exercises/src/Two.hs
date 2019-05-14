module Two 
     ( Two
     , TwoAssoc
     ) where

import Test.QuickCheck

data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Two x y

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two a b) <> (Two c d) = (Two (a <> c) (b <> d))

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool