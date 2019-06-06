-- stack ghc -- -prof -fprof-auto -rtsopts -O2 profilingHeap.hs
-- ./profilingHeap +RTS -hc -p
-- hp2ps profilingHeap.hp
module Main where

import Control.Monad

blah :: [Integer]
blah = [1..1000]

main :: IO ()
main = replicateM_ 10000 (print blah)
    