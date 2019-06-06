-- stack build criterion
-- stack ghc -- -O2 profilingUnboxed.hs
-- ./profilingUnboxed
module Main where

import Criterion.Main
import qualified Data.Vector as B (Vector, fromList, foldr)
import qualified Data.Vector.Unboxed as UB (Vector, fromList, foldr)

b :: B.Vector Int
b = B.fromList [1..10000]

ub :: UB.Vector Int
ub = UB.fromList [1..10000]

bSum :: B.Vector Int -> Int
bSum = B.foldr (+) 0

ubSum :: UB.Vector Int -> Int
ubSum = UB.foldr (+) 0

main :: IO ()
main = defaultMain
    [ bench "boxed vector" $
      whnf bSum b
    , bench "unboxed vector" $
      whnf ubSum ub
    ]