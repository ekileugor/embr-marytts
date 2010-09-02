module Threading where

import Control.Concurrent
import Control.Exception
import System.CPUTime

waitForChildren children = do
  cs <- takeMVar children
  case cs of
    []   -> return ()
    m:ms -> do
            putMVar children ms
            takeMVar m
            waitForChildren children

forkChild children io = do
  mvar <- newEmptyMVar
  childs <- takeMVar children
  putMVar children (mvar:childs)
  forkIO (io `finally` putMVar mvar ())


executeInParallel :: [IO ()]
                     -> IO ()
executeInParallel ls = do
  children <- newMVar []
  mapM_ (forkChild children) ls
  waitForChildren children 

waitMillis :: Integer -> IO ()
waitMillis millis = do
  t <- getCPUTime
  aux t where
    d = millis * 1000000000
    aux start = do
      t <- getCPUTime
      if (t-start) >= d then return () else aux start