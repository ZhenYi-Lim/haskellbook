module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar i x = chr ((ord first) + (mod ((ord x) + i - (ord first)) 26))
    where
        first = if isUpper x then 'A' else 'a'
        last = if isUpper x then 'Z' else 'z'

cipher :: Int -> String -> String
cipher i x = map (shiftChar i) x

unCipher :: Int -> String -> String
unCipher i x = cipher (negate i) x