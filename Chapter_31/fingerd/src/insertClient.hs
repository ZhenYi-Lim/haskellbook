{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket hiding (close, recv)
import Network.Socket.ByteString (recv, sendAll)
import System.Environment
import Data.List (intersperse)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

main :: IO ()
main = withSocketsDo $ do
    args <- getArgs
    addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                            (Just "localhost") (Just "78")
    let addr = head addrinfos
    sock <- socket (addrFamily addr) Stream defaultProtocol
    connect sock $ addrAddress addr
    putStrLn "Sending user data..."
    sendAll sock $ encodeUtf8 $ T.pack $ concat (intersperse " " args)
    msg <- recv sock 1024
    putStrLn $ T.unpack $ decodeUtf8 msg
    sClose sock