{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types

import Database

addUser :: Connection -> IO ()
addUser conn = do
    putStrLn "Adding new user."
    putStr "Login: "
    username' <- getLine
    putStr "Name: "
    name' <- getLine
    putStr "Directory: "
    homeDirectory' <- getLine
    putStr "Shell: "
    shell' <- getLine
    putStr "Phone: "
    phone' <- getLine
    execute conn insertUser (Null, username', shell', homeDirectory', name', phone')
    putStrLn "User added."

modUser :: Connection -> IO ()
modUser conn = do
    putStrLn "Modifying existing user."
    putStr "Username: "
    un <- getLine
    let username = T.pack un
    qUser <- getUser conn username
    case qUser of
        Nothing -> putStrLn "User not found. Exiting now."
        Just user -> do
            putStrLn "Which field would you like to modify?"
            putStrLn "Enter '1' for username, '2' for Shell, '3' for Home Directory, '4' for Name, '5' for Phone Number:"
            field <- getLine
            putStr "Please provide a new value for the field: "
            value <- getLine
            putStrLn value
            case field of
                "1" -> execute conn (modQueryUsername value) (Only username)
                "2" -> execute conn (modQueryShell value) (Only username)
                "3" -> execute conn (modQueryHome value) (Only username)
                "4" -> execute conn (modQueryName value) (Only username)
                "5" -> execute conn (modQueryPhone value) (Only username)
                _ -> do
                    putStrLn "Invalid selection. Exiting now."
                    return ()
            putStrLn "Modification completed. Exiting now."

main :: IO ()
main = do
    conn <- open "finger.db"
    putStr "Enter '1' to add a new user, '2' to modify an existing user: "
    mode <- getLine
    case mode of
        "1" -> addUser conn
        "2" -> modUser conn
        _ -> putStrLn "Invalid selection. Exiting now."
    SQLite.close conn