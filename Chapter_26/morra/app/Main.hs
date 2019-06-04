module Main where

import System.IO     (hSetBuffering, stdout, BufferMode(NoBuffering))
import Control.Monad.Trans.State (StateT, evalStateT)
import qualified Data.Map as M (empty)

import Common (GameState)
import SinglePlayer (singlePlayerGame)
import TwoPlayers (twoPlayersGame)

data GameChoice = SinglePlayer | TwoPlayers

selectMode :: IO (GameChoice)
selectMode = do
    putStr "\nPress '1' for Single Player, '2' for Two Players: "
    choice <- getLine
    case choice of
        "1" -> return (SinglePlayer)
        "2" -> return (TwoPlayers)
        _ -> do putStrLn "Invalid choice. Please try again."
                selectMode

startGame :: StateT GameState IO () -> IO ()
startGame = flip evalStateT ((0, 0), (Nothing, Nothing), M.empty)
--startGame = flip evalStateT (GameScore { p1 = 0, p2 = 0 })

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "\nPlay Morra"
    mode <- selectMode
    startGame $ case mode of
                    SinglePlayer -> singlePlayerGame
                    TwoPlayers -> twoPlayersGame