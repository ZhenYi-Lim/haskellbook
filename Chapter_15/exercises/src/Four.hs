module Four 
     ( Four
     , FourAssoc
     ) where

import Test.QuickCheck

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        w <- arbitrary
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Four w x y z

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
    (Four a b c d) <> (Four e f g h) = (Four (a <> e) (b <> f) (c <> g) (d <> h))

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
    mempty = Four mempty mempty mempty mempty

type FourAssoc = Four String String String String -> Four String String String String -> Four String String String String -> Bool