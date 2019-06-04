{-# LANGUAGE OverloadedStrings #-}

module Scotty where

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class

import Web.Scotty.Internal.Types (ActionT(..))
import Control.Monad (liftM)
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State.Lazy hiding (get)

main = scotty 3000 $ do
    --get :: RoutePattern -> ActionM () -> ScottyM ()
    get "/:word" $ do
        beam <- param "word"
        --lift (putStrLn "hello")
        --(ActionT . lift . lift . lift) (putStrLn "hello")
        --(ActionT . (ExceptT . liftM Right) . lift . lift) (putStrLn "hello")
        --(ActionT . (ExceptT . liftM Right) . ReaderT . const . lift) (putStrLn "hello")
        {- (ActionT . (ExceptT . liftM Right) . ReaderT . const . 
            \m -> StateT $ \s -> do
                a <- m
                return (a, s)) (putStrLn "hello") -}
        liftIO (putStrLn "hello")
        html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]