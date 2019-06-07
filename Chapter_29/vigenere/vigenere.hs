-- stack ghc -- O2 vigenere.hs
-- ./vigenere abcd -e
-- ./vigenere abcd -d
module Main where

import Data.Char
import System.Environment (getArgs)
import System.IO (hPutStr, hGetChar, stdout, stdin, hWaitForInput)

data Mode = Encrypt | Decrypt

--copied from Chapter 11
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

wordKeyMap3 :: Int -> [Int] -> [String] -> [String] -> (Int,[String],[String])
wordKeyMap3 _ _ [] wordIntList = (0, [], wordIntList)
wordKeyMap3 i keyInt wordList encryptedWordList = wordKeyMap3 i' keyInt wordList' encryptedWordList' 
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

vigenere :: Mode -> String -> String -> String  -- slight modification to allow for decryption
vigenere mode key message = unwords encryptedWordList
    where
        keyInt = case mode of
            Encrypt -> keyShift key -- key translted into shift number
            Decrypt -> map negate (keyShift key)
        wordList = words message    -- message broken into words
        (_,_,encryptedWordList) = wordKeyMap3 0 keyInt wordList []

getText :: String -> IO (String)
getText s = do
    c <- hGetChar stdin
    case c of
        '\n' -> return (reverse s)
        otherwise -> getText (c:s)

main :: IO ()
main = do
    args <- getArgs
    let key = head args
        mode = head (tail args)
        rest = drop 2 args
        wait = case rest of
            [] -> 10000 -- 10s
            otherwise -> (*1000) (read (head rest)) --exceptions not handled

    anyInput <- hWaitForInput stdin wait
    case anyInput of
        False -> putStrLn "Timed out."
        True -> do
            text <- getText ""
            case mode of
                "-e" -> do
                    hPutStr stdout (vigenere Encrypt key text)
                "-d" -> do
                    hPutStr stdout (vigenere Decrypt key text)