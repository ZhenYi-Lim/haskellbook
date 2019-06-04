module Common
     ( Score
     , GameScore
     --, GameScore (GameScore, p1, p2)
     , Move
     , MoveHistory
     , Memory
     , GameState
     , playerMove
     , roundResult
     ) where

import qualified Data.Map as M (Map)

type Score = Int
type GameScore = (Score, Score)
--data GameScore = GameScore { p1 :: Score, p2 :: Score }
type Move = Bool
type MoveHistory = (Maybe Move, Maybe Move)
type Memory = M.Map MoveHistory (IO Move)
type GameState = (GameScore, MoveHistory, Memory)

playerMove :: String -> String -> IO (Move)
playerMove name abrv = do
    putStr $ name ++ ", will your move be '0' or '1'?\n" ++ abrv ++ ": "
    m <- getLine
    case m of
        "0" -> return False
        "1" -> return True
        _ -> do
                putStrLn "Invalid move."
                playerMove name abrv

-- P1's move, P2's move, result is if P1 wins
roundResult :: Move -> Move -> Bool
roundResult = (/=)
-- 1 1 -> Even -> P1 loses
-- 0 1 -> Odd  -> P1 wins
-- 1 0 -> Odd
-- 0 0 -> Even
-- XOR