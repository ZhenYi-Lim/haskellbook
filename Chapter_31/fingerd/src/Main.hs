{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
--import Data.ByteString (ByteString)
--import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

import Database
    
returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
  maybeUser <- getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn ("Couldn't find matching user for username: " ++ (show username))
      return ()
    Just user -> sendAll soc (formatUser user)

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated = T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

handleQuery :: Connection -> Socket -> IO ()
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name -> returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Connection -> Socket -> IO ()
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling query"
  handleQuery dbConn soc
  sClose soc

handleInsertion :: Connection -> Socket -> IO ()
handleInsertion dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got connection, handling insertion"
  msg <- recv soc 1024
  case words (T.unpack $ decodeUtf8 msg) of
    [username, shell, homeDir, realName, phone] -> do
      execute dbConn insertUser userRow
      sendAll soc (encodeUtf8 "Insertion successful.")
          where userRow :: UserRow
                userRow = (Null, T.pack username, T.pack shell, T.pack homeDir, T.pack realName, T.pack phone)
    _ -> do
      putStrLn "Invalid input. Unable to insert new user."
      sendAll soc (encodeUtf8 "Invalid input. Unable to insert new user.")
  sClose soc

startServer :: String -> (Connection -> Socket -> IO a) -> IO ()
startServer port handler = do
  addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                            Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handler conn sock
  SQLite.close conn
  sClose sock

main :: IO ()
main = withSocketsDo $ do
  _ <- forkIO $ startServer "78" handleInsertion
  startServer "79" handleQueries
  {- addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                            Nothing (Just "79")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  -- only one connection open at a time
  conn <- open "finger.db"
  handleQueries conn sock
  SQLite.close conn
  sClose sock -}
  