-- \ f a b c -> f a (f b c) == f (f a b) c
-- \ (<>) a b c -> a <> (b <> c) == (a <> b) <> c

{- asc :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
asc (<>) a b c = a <> (b <> c) == (a <> b) <> c -}

--in main project directory run:
--stack ghci monoid-quickcheck:tests
--type S = String
--type B = Bool
--quickCheck (monoidAssoc :: S -> S -> S -> B)

--Changed definition of Monoid.
--https://stackoverflow.com/questions/53022425/data-constructor-not-in-scope-monoid-bull-testbatch

module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck
import Bull
import Optional
import First'

monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
    let ma = monoidAssoc
        mli = monoidLeftIdentity
        mlr = monoidRightIdentity
    quickCheck (ma :: BullMappend)
    quickCheck (mli :: Bull -> Bool)
    quickCheck (mlr :: Bull -> Bool)
    quickCheck (monoidAssoc :: FirstMappend)
    quickCheck (monoidLeftIdentity :: FstId)
    quickCheck (monoidRightIdentity :: FstId)