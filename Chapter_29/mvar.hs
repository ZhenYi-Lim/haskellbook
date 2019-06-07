module WhatHappens where

import Control.Concurrent
import System.IO.Unsafe

myData :: IO (MVar Int)
myData = newEmptyMVar

myData' :: MVar Int
myData' = unsafePerformIO newEmptyMVar

main :: IO ()
main = do
{-  mv <- myData
    putMVar mv 0
    mv' <- myData
    zero <- takeMVar mv'
    print zero -}
    
    {- mv <- newEmptyMVar
    putMVar mv (0 :: Int)
    zero <- takeMVar mv
    print zero -}
    
    putMVar myData' 0
    zero <- takeMVar myData'
    print zero
