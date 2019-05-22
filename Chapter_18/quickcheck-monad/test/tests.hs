module Main where

--import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Sum
import BadMonad
import Nope
import PhhhbbtttEither
import Identity
import Lst
    
main :: IO ()
main = do
    putStrLn "\nSum"
    quickBatch (functor (undefined :: Sum String (Int, Int, Int)))
    quickBatch (applicative (undefined :: Sum String (Int, Int, Int)))
    quickBatch (monad (undefined :: Sum String (Int, Int, Int)))
    putStrLn "\nBadMonad"
    let trigger :: CountMe (Int, String, Int)
        trigger = undefined
    quickBatch $ functor trigger
    quickBatch $ applicative trigger
    quickBatch $ monad trigger
    putStrLn "\nNope"
    quickBatch $ functor (undefined :: Nope (Int, Int, Int))
    quickBatch $ applicative (undefined :: Nope (Int, Int, Int))
    quickBatch $ monad (undefined :: Nope (Int, Int, Int))
    putStrLn "\nPhhhbbtttEither"
    quickBatch $ functor (undefined :: PhhhbbtttEither String (Int, Int, Int))
    quickBatch $ applicative (undefined :: PhhhbbtttEither String (Int, Int, Int))
    quickBatch $ monad (undefined :: PhhhbbtttEither String (Int, Int, Int))
    putStrLn "\nIdentity"
    quickBatch $ functor (undefined :: Identity (Int, Int, Int))
    quickBatch $ applicative (undefined :: Identity (Int, Int, Int))
    quickBatch $ monad (undefined :: Identity (Int, Int, Int))
    putStrLn "\nLst"
    quickBatch $ functor (undefined :: Lst (Int, Int, Int))
    quickBatch $ applicative (undefined :: Lst (Int, Int, Int))
    quickBatch $ monad (undefined :: Lst (Int, Int, Int))