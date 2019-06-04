{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
    Config {
        -- that's one, one click!
        -- two...two clicks!
        -- Three BEAUTIFUL clicks! ah ah ahhhh
        counts :: IORef (M.Map Text Integer)
        , prefix :: Text
    }

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = let val = fromMaybe 0 (M.lookup k m)
                in (M.insert k (val+1) m, (val+1))

app :: Scotty ()
app = get "/:key" $ do
        config <- lift ask
        unprefixed <- param "key"
        ref <- liftIO (readIORef (counts config))
        let key' = mappend (prefix config) unprefixed
            (newMap, newInteger) = bumpBoomp key' ref
        liftIO (writeIORef (counts config) newMap)
        html $ mconcat [ "<h1>Success! Count was: "
                        , TL.pack $ show newInteger
                        , "</h1>" ]

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config { counts = counter, prefix = TL.pack prefixArg }
        runR = flip runReaderT config
    scottyT 3000 runR app