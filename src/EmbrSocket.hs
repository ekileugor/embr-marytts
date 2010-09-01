module EmbrSocket where

import Network.Fancy
import Embr
import System.IO
import Data.Monoid

standardEmberServer :: Address
standardEmberServer = IP "localhost" 5555

sendCommands :: Command -> Handle -> IO ()
sendCommands c h = do
  hPutStrLn h $ show c
  hFlush h 
  
standardAct :: Command -> IO ()
standardAct c = withStream standardEmberServer (sendCommands (TimeReset `mappend` c))               

rawAct :: String -> IO ()
rawAct s = withStream standardEmberServer f where
  f h = do
    hPutStr h s
    hFlush h
    
actFromFile :: FilePath -> IO ()
actFromFile f = do
  s <- readFile f
  rawAct s