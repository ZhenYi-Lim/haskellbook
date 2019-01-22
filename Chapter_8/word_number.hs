module WordNumber where

import Data.List (intersperse)

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
digitToWord _ = "error"

digits :: Int -> [Int]
digits num = go num []
    where go n list
            | n < 10 = [n] ++ list
            | otherwise = go (div n 10) ([(mod n 10)] ++ list)

wordNumber :: Int -> String
wordNumber n = concat (intersperse "-" wordList)
    where wordList = map digitToWord (digits n)