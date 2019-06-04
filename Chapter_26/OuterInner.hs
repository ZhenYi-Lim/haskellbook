module OuterInner where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader

-- We only need to use return once
-- because it's one big Monad
embedded :: MaybeT 
            (ExceptT String 
                     (ReaderT () IO))
            Int
embedded = return 1
    
maybeUnwrap :: ExceptT String
                       (ReaderT () IO)
                       (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO
                (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

readerUnwrap :: () -> 
                IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

embedded' :: MaybeT
            (ExceptT String
                     (ReaderT () IO))
            Int
embedded' = undefined
--missing something here.
--embedded' = MaybeT $ ExceptT $ ReaderT $ \() -> (const (Right (Just (1::Int))))