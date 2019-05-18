module Main where

import Test.QuickCheck

import FunctorLaws
import Identity
import Pair
import Two
import Three
import Three'
import Four
import Four'
import BoolAndSomethingElse
import BoolAndMaybeSomethingElse
import Sum
import Company
import More
import Quant
import K
import K'
import EvilGoateeConst

f :: [Int] -> Bool
f x = functorIdentity x

c = functorCompose (+1) (*2)

li x = c (x :: [Int])

main :: IO ()
main = do
    putStrLn "Test"
    quickCheck ( f )
    quickCheck ( li )
    putStrLn "Identity"
    quickCheck ( functorIdentity :: Identity Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Identity Int -> Bool)
    putStrLn "Pair"
    quickCheck ( functorIdentity :: Pair Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Pair Int -> Bool )
    putStrLn "Two"
    quickCheck ( functorIdentity :: Two String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Two String Int -> Bool )
    putStrLn "Three"
    quickCheck ( functorIdentity :: Three String Bool Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Three String Bool Int -> Bool )
    putStrLn "Three'"
    quickCheck ( functorIdentity :: Three' String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Three' String Int -> Bool )
    putStrLn "Four"
    quickCheck ( functorIdentity :: Four String Bool Char Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Four String Bool Char Int -> Bool )
    putStrLn "Four'"
    quickCheck ( functorIdentity :: Four' String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Four' String Int -> Bool )
    putStrLn "Not possible for Trivial, as data Trivial = Trivial has kind *."
    putStrLn "Not possible for Bool"
    putStrLn "BoolAndSomethingElse"
    quickCheck ( functorIdentity :: BoolAndSomethingElse Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: BoolAndSomethingElse Int -> Bool )
    putStrLn "BoolAndMaybeSomethingElse"
    quickCheck ( functorIdentity :: BoolAndMaybeSomethingElse Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: BoolAndMaybeSomethingElse Int -> Bool )
    putStrLn "Not possible for Mu, has kind (* -> *) -> *"
    putStrLn "Not possible for D, type is fully defined (has kind *)"
    putStrLn "Sum"
    quickCheck ( functorIdentity :: Sum String Int -> Bool )
    quickCheck (functorCompose (+1) (+2) :: Sum String Int -> Bool )
    putStrLn "Company"
    quickCheck ( functorIdentity :: Company String Bool Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Company String Bool Int -> Bool )
    putStrLn "More"
    quickCheck ( functorIdentity :: More String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: More String Int -> Bool )
    putStrLn "Quant"
    quickCheck ( functorIdentity :: Quant String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: Quant String Int -> Bool )
    putStrLn "K"
    quickCheck ( functorIdentity :: K String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: K String Int -> Bool )
    --quickCheck ( functorIdentity :: Flip K Int String -> Bool )
    --quickCheck ( functorCompose (+1) (+2) :: Flip K Int String -> Bool )
    putStrLn "EvilGoateeConst"
    quickCheck ( functorIdentity :: EvilGoateeConst String Int -> Bool )
    quickCheck ( functorCompose (+1) (+2) :: EvilGoateeConst String Int -> Bool )