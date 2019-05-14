module BoolConj 
     ( BoolConj
     , BoolConjAssoc
     ) where

import Test.QuickCheck

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = do
        x <- arbitrary
        return $ BoolConj x

instance Semigroup BoolConj where
    (BoolConj True) <> (BoolConj True) = (BoolConj True)
    _ <> _ = (BoolConj False)

instance Monoid BoolConj where
    mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool