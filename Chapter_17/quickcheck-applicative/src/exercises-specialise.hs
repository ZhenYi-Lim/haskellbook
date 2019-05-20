 -- Type []
pure :: a -> [a]
(<*>) :: [(a -> b)] -> [a] -> [b]
-- Type IO
pure :: a -> IO a
(<*>) :: IO (a -> b) -> IO a -> IO b
-- Type (,) a
pure :: a -> (,) [Char] a
(<*>) :: ((,) [Char] (a -> b) ) -> ( (,) [Char] a ) -> ( (,) [Char] b )
-- Type (->) e
pure :: a -> (->) Int a
(<*>) :: ( (->) Int (a -> b) ) -> ( (->) Int a ) -> ( (->) Int b )
