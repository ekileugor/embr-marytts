module Threading where

import Control.Concurrent
import Control.Exception

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


test name = aux 10000 where
    aux 0 = return ()
    aux n = do
      putStrLn $ name ++ " " ++ show n
      aux (n-1)
