module Three 
     ( Three
     , ThreeAssoc
     ) where

import Test.QuickCheck

data Three a b c = Three a b c deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        z <- arbitrary
        return $ Three x y z

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
    (Three a b c) <> (Three d e f) = (Three (a <> d) (b <> e) (c <> f))

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
    mempty = Three mempty mempty mempty

type ThreeAssoc = Three String String String -> Three String String String -> Three String String String -> Bool