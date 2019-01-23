module Main where

import Control.Monad (forever) -- [1]
import Data.Char (toLower) -- [2]
import Data.Maybe (isJust) -- [3]
import Data.List (intersperse) -- [4]
import System.Exit (exitSuccess) -- [5]
import System.Random (randomRIO) -- [6]
import System.IO

--type WordList = [String]
newtype WordList = WordList [String]
  deriving (Eq, Show)

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  --return (lines dict)
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  --aw <- allWords
  (WordList aw) <- allWords
  --return (filter gameLength aw)
  return $ WordList (filter gameLength aw)
    where 
      gameLength :: String -> Bool
      gameLength w =
        let l = length (w :: String)
        in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
--randomWord wl = do
randomWord (WordList wl) = do
  randomIndex <- randomRIO ( 0 , (length wl)-1 )
  return $ wl !! randomIndex
        
randomWord' :: IO String
randomWord' = gameWords >>= randomWord

--Word to be guessed, characters filled so far, letters guessed so far.
data Puzzle = Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guesssed _) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered)

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word nothings "" 0
    where
      nothings = map (\c -> Nothing) word

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _ _) c = elem c word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) c = elem c guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Int -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s _) c i =
  Puzzle word newFilledInSoFar (c:s) i
    where
      zipper guessed wordChar guessChar =
        if wordChar == guessed
          then Just wordChar
        else guessChar
      newFilledInSoFar = zipWith (zipper c) word filledInSoFar
      
handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle@(Puzzle _ _ _ i) guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that\
      \ character, pick \
      \ something else!"
      putStr "Number of chances left: "
      print (chances - i + 1)
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the\
      \ word, filling in the word\
      \ accordingly"
      putStr "Number of chances left: "
      print (chances - i + 1)
      return (fillInCharacter puzzle guess i)
    (False, _) -> do
      putStrLn "This character wasn't in\
      \ the word, try again."
      putStr "Number of chances left: "
      print (chances - i)
      return (fillInCharacter puzzle guess (i+1))

chances :: Int
chances = 7

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed i) =
  if i > chances
  then do
    putStrLn "You lose!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle wordToGuess filledInSoFar _ _) =
  if all isJust filledInSoFar
  then do
    putStrLn "You win!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess
  else return ()
  
runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle