import Control.Monad
import System.Exit (exitSuccess)
import Data.Char (toLower)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    let
        line2 = map toLower line1
        line3 = filter (\x -> elem x ['a'..'z']) line2
    case (line3 == reverse line3) of
        True -> putStrLn "It's a palindrome!"
        False -> putStrLn "Nope!"
    exitSuccess