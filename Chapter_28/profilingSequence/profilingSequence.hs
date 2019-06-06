-- stack build criterion
-- stack ghc -- -O2 profilingSequence.hs
-- ./profilingSequence
module Main where
    
import Criterion.Main
import qualified Data.Sequence as S

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [S.Seq Int]
seqs = replicate 10 (S.fromList [1..100000])

lists1 :: [Int]
lists1 = [1..100000]

seqs1 :: S.Seq Int
seqs1 = S.fromList [1..100000]


main :: IO ()
main = defaultMain
    [ bench "concatenate lists" $
      nf mconcat lists
    , bench "concatenate sequences" $
      nf mconcat seqs
    , bench "indexing list" $
      whnf (\xs -> xs !! 9001) lists1
    , bench "indexing sequence" $
      whnf (flip S.index 9001) seqs1
    ]