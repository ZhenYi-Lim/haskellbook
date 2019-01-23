module Main where

import Cipher
import Control.Monad (forever)
import System.Exit (exitSuccess)
import System.IO

chooseCipher :: IO()
chooseCipher = forever $ do
  putStrLn "Which encrption method do you want to use?"
  putStrLn "a: Caesar"
  putStrLn "b: Vigenere"
  putStr "Choice: "
  choice <- getLine
  case choice of
    "a" -> runCaesar
    "b" -> runVigenere
    _ -> putStrLn "Invalid choice."

runCaesar :: IO()
runCaesar = do
  putStr "Text to encode: "
  text <- getLine
  putStr "Shift number: "
  shiftStr <- getLine
  case all (\x -> elem x ['0'..'9']) shiftStr of
    True -> putStrLn $ "Encrypted text is: " ++ (caesar (read shiftStr :: Int) text)
    False -> putStrLn "Invalid choice."
  exitSuccess

runVigenere :: IO()
runVigenere = do
  putStr "Text to encode: "
  text <- getLine
  putStr "Key text: "
  key <- getLine
  putStrLn $ "Encrypted text is: " ++ (vigenere key text)
  exitSuccess

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  chooseCipher
