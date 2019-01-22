module Exercises where
    thirdLetter :: Int -> Char
    thirdLetter x = "Curry is awesome!" !! (x-1)

    rvrs :: String -> String
    rvrs x = (drop 9 x) ++ take 4 (drop 5 x) ++ (take 5 x)
