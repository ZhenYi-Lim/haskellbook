module EvilGoateeConst
     ( EvilGoateeConst (GoatyConst)
     ) where

import Test.QuickCheck

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)

instance Arbitrary b => Arbitrary (EvilGoateeConst a b) where
    arbitrary = do
        x <- arbitrary
        return $ GoatyConst x

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst x) = GoatyConst (f x)