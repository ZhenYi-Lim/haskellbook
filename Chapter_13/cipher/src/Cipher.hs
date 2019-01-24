module Cipher where

import Data.Char

shiftChar :: Int -> Char -> Char
shiftChar i x = chr ((ord first) + (mod ((ord x) + i - (ord first)) 26))
    where
        first = if isUpper x then 'A' else 'a'
        last = if isUpper x then 'Z' else 'z'

caesar :: Int -> String -> String
caesar i x = map (shiftChar i) x

unCaesar :: Int -> String -> String
unCaesar i x = caesar (negate i) x

keyShift :: String -> [Int]
keyShift key = map shift key
        where
            shift x =   if isUpper x
                        then (ord x) - (ord 'A')
                        else (ord x) - (ord 'a')

wordKeyMap :: Int -> [Int] -> [String] -> [String] -> (Int,[String],[String])
wordKeyMap _ _ [] wordIntList = (0, [], wordIntList)
wordKeyMap i keyInt wordList encryptedWordList = wordKeyMap i' keyInt wordList' encryptedWordList' 
        where
            word = head wordList
            keyLength = length keyInt
            wordLength = length word
            charNum = map (+i) [0..(wordLength-1)]       -- list of ints for position of character in message (excludes spaces)
            charNumKey = map (`mod` keyLength) charNum      -- list indicating which key character each character is mapped onto.
            shiftMap = map (keyInt !!) charNumKey       -- list of shift number for the word
            encryptedWordList' = encryptedWordList ++ [zipWith (shiftChar) shiftMap word]   -- encrypt word and add to list of encrpted words.
            wordList' = tail wordList       -- remaining words to be encrypted
            i' = i + length word            -- update i start position for next recursion

vigenere :: String -> String -> String
vigenere key message = unwords encryptedWordList
    where 
        keyInt = keyShift key   -- key translted into shift number
        wordList = words message    -- message broken into words
        (_,_,encryptedWordList) = wordKeyMap 0 keyInt wordList []

unVigenere :: String -> String -> String
unVigenere key message = unwords encryptedWordList
    where 
        keyInt = map negate (keyShift key)   -- key translted into shift number
        wordList = words message    -- message broken into words
        (_,_,encryptedWordList) = wordKeyMap 0 keyInt wordList []