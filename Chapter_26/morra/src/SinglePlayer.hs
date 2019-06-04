--https://wiki.haskell.org/Simple_StateT_use
module SinglePlayer
     ( singlePlayerGame
     ) where

import Control.Monad.Trans.State (StateT (StateT), get, put)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as M (findWithDefault, insert)
import System.Random (randomIO)

import Common (GameState, MoveHistory, Move, Memory, playerMove, roundResult)

singlePlayerGame :: StateT GameState IO ()
singlePlayerGame = do
    lift $ putStrLn "\nPlayer is P1, playing for Odds\nComputer is P2, playing for Evens."
    ((sP1, sP2), (m1, m2), mem)  <- get
    mC <- lift $ computerMove (m1, m2) mem
    mP1 <- lift $ playerMove "Player" "P1"
    lift $ putStrLn $ "P2: " ++ show (fromEnum mC)
    case roundResult mP1 mC of
        False -> do
            lift $ putStrLn "Computer wins!"
            put ((sP1, sP2+1), (m2, Just mP1), M.insert (m1, m2) (return mP1) mem)
            lift $ putStrLn $ "P1 Score: " ++ show sP1 ++ ", P2 Score: " ++ show (sP2+1)
        True -> do
            lift $ putStrLn "Player wins!"
            put ((sP1+1, sP2), (m2, Just mP1), M.insert (m1, m2) (return mP1) mem)
            lift $ putStrLn $ "P1 Score: " ++ show (sP1+1) ++ ", P2 Score: " ++ show sP2
    lift $ putStr $ "Keep playing? Enter 'n' to quit: "
    choice <- lift getLine
    case choice of 
        "n" -> lift $ putStrLn "Thanks for playing!"
        otherwise -> singlePlayerGame

computerMove :: MoveHistory -> Memory -> IO Move
computerMove (Nothing, _) _ = randomIO
computerMove mh mem = M.findWithDefault randomIO mh mem --match player's predicted move to win
