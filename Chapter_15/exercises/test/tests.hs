module Main where

import Control.Monad
import Data.Monoid
import Test.QuickCheck

import Trivial
import Identity
import Two
import Three
import Four
import BoolConj
import BoolDisj
import Or
import Combine
import Comp
import Validation
import Mem

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

combineSemigroupAssoc :: (Eq b, Semigroup b) => Combine a b -> Combine a b -> Combine a b -> a -> Bool
combineSemigroupAssoc a b c n = (unCombine (a <> (b <> c)) n)  == (unCombine ((a <> b) <> c) n)

combineMonoidLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidLeftIdentity a n = (unCombine (mempty <> a) n) == (unCombine a n)

combineMonoidRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidRightIdentity a n = (unCombine (a <> mempty) n) == (unCombine a n)

compSemigroupAssoc :: (Eq a, Semigroup a) => Comp a -> Comp a -> Comp a -> a -> Bool
compSemigroupAssoc a b c n = (unComp (a <> (b <> c)) n) == (unComp ((a <> b) <> c) n)

compMonoidLeftIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compMonoidLeftIdentity a n = (unComp (mempty <> a) n) == (unComp a n)

compMonoidRightIdentity :: (Eq a, Monoid a) => Comp a -> a -> Bool
compMonoidRightIdentity a n = (unComp (mempty <> a) n) == (unComp a n)

main :: IO ()
main = do
    putStrLn "Trivial"
    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (monoidLeftIdentity :: Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Trivial -> Bool)
    putStrLn "Identity"
    quickCheck (semigroupAssoc :: IdenAssoc)
    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)
    putStrLn "Two"
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (monoidLeftIdentity :: Two String String -> Bool)
    quickCheck (monoidRightIdentity :: Two String String -> Bool)
    putStrLn "Three"
    quickCheck (semigroupAssoc :: ThreeAssoc)
    quickCheck (monoidLeftIdentity :: Three String String String -> Bool)
    quickCheck (monoidRightIdentity :: Three String String String -> Bool)
    putStrLn "Four"
    quickCheck (semigroupAssoc :: FourAssoc)
    quickCheck (monoidLeftIdentity :: Four String String String String -> Bool)
    quickCheck (monoidRightIdentity :: Four String String String String -> Bool)
    putStrLn "BoolConj"
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)
    putStrLn "BoolDisj"
    quickCheck (semigroupAssoc :: BoolDisjAssoc)
    quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
    quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
    putStrLn "Or"
    quickCheck (semigroupAssoc :: OrAssoc)
    putStrLn "Combine"
    quickCheck (combineSemigroupAssoc :: CombineAssoc)
    quickCheck (combineMonoidLeftIdentity :: Combine Bool (Product Int) -> Bool -> Bool)
    quickCheck (combineMonoidRightIdentity :: Combine Bool (Product Int) -> Bool -> Bool)
    putStrLn "Comp"
    quickCheck (compSemigroupAssoc :: CompAssoc)
    quickCheck (compMonoidLeftIdentity :: Comp (Product Int) -> Product Int -> Bool)
    quickCheck (compMonoidRightIdentity :: Comp (Product Int) -> Product Int -> Bool)
    putStrLn "Validation"
    let failure :: String -> Validation String Int
        failure = Validation.Failure
        success :: Int -> Validation String Int
        success = Validation.Success
    print $ success 1 <> failure "blah"
    print $ failure "woot" <> failure "blah"
    print $ success 1 <> success 2
    print $ failure "woot" <> success 2
    quickCheck (semigroupAssoc :: ValidationAssoc)
    quickCheck (monoidLeftIdentity :: Validation String Int -> Bool)
    quickCheck (monoidRightIdentity :: Validation String Int -> Bool)
    putStrLn "Mem"
    let rmzero = runMem mempty 0
        f' = Mem (\s -> ("hi", s + 1))
        rmleft = runMem (f' <> mempty) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0
