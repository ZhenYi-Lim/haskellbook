-- stack build criterion
-- stack ghc -- -O2 benchIndex.hs
-- ./benchIndex
module Main where

import Criterion.Main
import Debug.Trace

{- infixl 9 !?
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n-1) -}

infixl 9 !?
{-# INLINABLE (!?) #-}
(!?) :: [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise =
    foldr
    (\x r k ->
      case k of
        0 -> Just x
        _ -> r (k-1))
    (const Nothing) xs n

myList :: [Int]
myList = [1..9999]

myList' :: [Int]
myList' = trace "myList was evaluated"
          ([1..9999] ++ [undefined])

myList'' = (undefined : [2..9999])

myList''' :: [Int]
myList''' = (undefined : undefined)

myList'''' :: [Int]
myList'''' = undefined

main0 :: IO ()
main0 = defaultMain
    [ bench "index list 9999"
      $ whnf (myList' !!) 9998
    , bench "index list maybe index 9999"
      -- $ whnf (myList' !?) 9998
      $ nf (myList' !?) 9999
    ]

main :: IO ()
main = defaultMain
  [ bench "map myList 9999" $
    whnf (map (+1)) myList
  , bench "map myList' 9999" $
    whnf (map (+1)) myList'
  , bench "map myList'' 9999" $
    whnf (map (+1)) myList''
  , bench "map myList''' 9999" $
    whnf (map (+1)) myList'''
  , bench "map myList'''' 9999" $
    whnf (map (+1)) myList''''
  ]