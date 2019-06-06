-- stack build criterion
-- stack ghc -- -O2 profilingSet.hs
-- ./profilingSet
module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)
bumpIt2 (i, v) = (i + 2, v + 2)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
    where stream = iterate bumpIt (0, 0)
    
s :: S.Set Int
s = S.fromList $ take 10000 stream
    where stream = iterate (+1) 0

membersMap :: Int -> Bool
membersMap i = M.member i m

membersSet :: Int -> Bool
membersSet i = S.member i s

m1 :: M.Map Int Int
m1 = M.fromList $ take 10000 stream
     where stream = iterate bumpIt2 (0, 0)

m2 :: M.Map Int Int 
m2 = M.fromList $ take 10000 stream
     where stream = iterate bumpIt2 (1, 1)

s1 :: S.Set Int
s1 = S.fromList $ take 10000 stream
     where stream = iterate (+2) 0

s2 :: S.Set Int
s2 = S.fromList $ take 10000 stream
     where stream = iterate (+2) 1

unionMap :: M.Map Int Int -> M.Map Int Int -> M.Map Int Int 
unionMap = M.union

unionSet :: S.Set Int -> S.Set Int -> S.Set Int
unionSet = S.union

main :: IO ()
main = defaultMain
    [ bench "member check map" $
      whnf membersMap 9999
    , bench "member check set" $
      whnf membersSet 9999
    , bench "member union map" $
      whnf (unionMap m1) m2
    , bench "member union set" $
      whnf (unionSet s1) s2
    ]
    