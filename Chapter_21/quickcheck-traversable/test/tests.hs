module Main where

--import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Identity
import Constant
import Optional
import Lst
import Three
import Pair
import Big
import Bigger
import SkiFree
import Tree

--Can't get this to work.
--traverseNaturality :: (Traversable t, Applicative f, Applicative g) => (a -> f b) -> (f b -> g b) -> t a -> Bool 
--traverseNaturality f t x = (t . traverse f) x == traverse (t . f) x

--t = undefined :: (Maybe Int -> Maybe Float)
--f = undefined :: (Integer -> Maybe Int)
--t.f :: (Integer -> Maybe Float)

--traverse :: (Traversable t, Applicative f) =>
--(a -> f b) -> t a -> f (t b)

main :: IO ()
main = do
    putStrLn "\nIdentity"
    quickBatch (traversable (undefined :: Identity (Int, Int, [Int])))
    putStrLn "\nConstant"
    quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))
    putStrLn "\nOptional"
    quickBatch (traversable (undefined :: Optional (Int, Int, [Int])))
    putStrLn "\nLst"
    quickBatch (traversable (undefined :: Lst (Int, Int, [Int])))
    putStrLn "\nThree"
    quickBatch (traversable (undefined :: Three Int Int (Int, Int, [Int])))
    putStrLn "\nPair"
    quickBatch (traversable (undefined :: Pair Int (Int, Int, [Int])))
    putStrLn "\nBig"
    quickBatch (traversable (undefined :: Big Int (Int, Int, [Int])))
    putStrLn "\nBigger"
    quickBatch (traversable (undefined :: Bigger Int (Int, Int, [Int])))
    putStrLn "\nS"
    --not sure what's up with this.
    --sample' (arbitrary :: Gen (S [] Int))
    putStrLn "\nTree"
    quickBatch (traversable (undefined :: Tree (Int, Int, [Int])))