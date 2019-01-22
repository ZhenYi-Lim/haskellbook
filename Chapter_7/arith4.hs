module Arith4 where
-- id :: a -> a
-- id x = x
roundTrip :: (Show a, Read b) => a -> b
roundTrip a = read (show a)

main :: IO()
main = do
    print ((roundTrip 4) :: Int)
    print (id 4)