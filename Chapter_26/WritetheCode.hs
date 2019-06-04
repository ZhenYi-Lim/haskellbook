import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity (Identity)

rDec :: Num a => Reader a a
rDec = reader $ \r -> r - 1

rDec' :: Num a => Reader a a
rDec' = reader $ subtract 1

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> return (show a)

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT $ return . show

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \r -> do
                    putStrLn ("Hi: " ++ (show r))
                    return (r + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
                    putStrLn ("Hi: " ++ (show s))
                    return (show s, s + 1)
