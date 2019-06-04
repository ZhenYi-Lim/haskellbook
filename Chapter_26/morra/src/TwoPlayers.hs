module TwoPlayers
     ( twoPlayersGame
     ) where

import Control.Monad.Trans.State (StateT (StateT), get, put)
import Control.Monad.Trans.Class (lift)
import System.Console.ANSI (clearScreen)

import Common (GameState, Move, playerMove, roundResult)

twoPlayersGame :: StateT GameState IO ()
twoPlayersGame = do
     ((sP1, sP2), hist, mem)  <- get
     lift clearScreen
     lift showAims
     mP1 <- lift $ playerMove "Player 1" "P1"
     lift clearScreen
     lift showAims
     mP2 <- lift $ playerMove "Player 2" "P2"
     lift clearScreen
     lift showAims
     lift (showRound mP1 mP2)
     case roundResult mP1 mP2 of
          False -> do
              lift $ putStrLn "Player 2 wins!"
              put ((sP1, sP2+1), hist, mem)
              lift $ putStrLn $ "P1 Score: " ++ show sP1 ++ ", P2 Score: " ++ show (sP2+1)
          True -> do
              lift $ putStrLn "Player 1 wins!"
              put ((sP1+1, sP2), hist, mem)
              lift $ putStrLn $ "P1 Score: " ++ show (sP1+1) ++ ", P2 Score: " ++ show sP2
     lift $ putStr $ "Keep playing? Enter 'n' to quit: "
     choice <- lift getLine
     case choice of 
          "n" -> lift $ putStrLn "Thanks for playing!"
          otherwise -> twoPlayersGame

showRound :: Move -> Move -> IO ()
showRound mP1 mP2 = do
     putStrLn $ "This round:"
     putStrLn $ "P1: " ++ show (fromEnum mP1)
     putStrLn $ "P2: " ++ show (fromEnum mP2)

showAims :: IO ()
showAims = putStrLn "\nPlayer1 is P1, playing for Odds\nPlayer2 is P2, playing for Evens."