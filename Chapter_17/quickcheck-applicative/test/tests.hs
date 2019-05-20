module Main where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import BadMonoid
import Apl1
import Lst
import ZipLst'
import Validation'
import Pair
import Two
import Three
import Three'
import Four
import Four'

main :: IO ()
main = do
    putStrLn "\nBadMonoid"
    quickBatch (monoid Twoo)
    putStrLn "\nApplicative"
    quickBatch (applicative [("b", "w", 1 :: Int)])
    let trigger :: [(String, String, Int)]
        trigger = undefined
    quickBatch (applicative trigger)
    putStrLn "\nZipLst"
    let zl = ZipLst [1 :: Sum Int]
    --quickBatch (monoid zl) --will not end with mempty = ZipLst (repeat mempty)
    putStrLn "\nLst"
    quickBatch (applicative (undefined :: Lst (Int, Int, Int)))
    --putStrLn "\nLst'"
    --quickBatch (applicative (undefined :: Lst' (Int, Int, Int)))
    putStrLn "\nZipLst'"
    quickBatch (applicative (undefined :: ZipLst' (Int, Int, Int)))
    putStrLn "\nValidation'" --Checking of id law doesn't complete. Perhaps due to the generation of [String]?
    --quickBatch (applicative (undefined :: Validation' [String] (Int, Int, Int)))
    putStrLn "\nPair"
    quickBatch (monoid (undefined :: Pair String))
    quickBatch (applicative (undefined :: Pair (Int, Int, Int)))
    putStrLn "\nTwo"
    quickBatch (monoid (undefined :: Two String String))
    quickBatch (applicative (undefined :: Two String (Int, Int, Int)))
    putStrLn "\nThree"
    quickBatch (monoid (undefined :: Three String String String))
    quickBatch (applicative (undefined :: Three String String (Int, Int, Int)))
    putStrLn "\nThree'"
    quickBatch (monoid (undefined :: Three' String String))
    quickBatch (applicative (undefined :: Three' String (Int, Int, Int)))
    putStrLn "\nFour"
    quickBatch (monoid (undefined :: Four String String String String))
    quickBatch (applicative (undefined :: Four String String String (Int, Int, Int)))
    putStrLn "\nFour'"
    quickBatch (monoid (undefined :: Four' String String))
    quickBatch (applicative (undefined :: Four' String (Int, Int, Int)))