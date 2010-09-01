module SimpleSound where

import Foreign
import Foreign.Marshal.Array
import Foreign.Storable
import Foreign.Ptr
import Sound.OpenAL

import Data.Word
import Data.Char
import Data.ByteString (ByteString,unpack)

setupStandardListener :: IO ()
setupStandardListener = do
  listenerPosition $= Vertex3 0 0 (0 :: ALfloat)
  listenerVelocity $= Vector3 0 0 (0 :: ALfloat)
  orientation $= (Vector3 0 0 ((-1) :: ALfloat),
                  Vector3 0 1 (0 :: ALfloat))
    
setupStandardSource :: Source -> Buffer -> IO ()
setupStandardSource source buf = do
  sourcePosition source $= Vertex3 0 0 (0 :: ALfloat)
  sourceVelocity source $= Vector3 0 0 (0 :: ALfloat)
  sourceGain source $= 1
  pitch source $= 1
  buffer source $= Just buf
  

-- | 'placeDataInNewBuffer d' creates a new buffer, puts the data stored in the string 'd' in the buffer and returns the buffer
placeDataInNewBuffer :: ByteString -- ^ The data as returned by MaryTTS
                        -> IO (Buffer,Ptr Word8) -- ^ The returned buffer
placeDataInNewBuffer d = do
  d' <- return $ unpack d
  [buf] <- genObjectNames 1
  ptrData <- mallocArray (length d')
  pokeArray ptrData d'
  bufferData buf $= BufferData (MemoryRegion ptrData (fromIntegral $ (length d') * (sizeOf $ head d'))) Mono16 16000
  return (buf,ptrData)
  
prepareToPlay :: ByteString -- ^ The sound passed as character data
                 -> IO (Device,Context,Source,Ptr Word8,Buffer)
prepareToPlay sound = do
  dev <- openDevice Nothing
  case dev of
    Nothing -> error $ "openDevice :::: " ++ errMsg 
    Just dev -> do
      cont <- createContext dev []
      case cont of
        Nothing -> error $ "createContext :::: " ++ errMsg
        Just c -> do
          currentContext $= cont
          setupStandardListener
          [s] <- genObjectNames 1
          (buf,ptr) <- placeDataInNewBuffer sound
          setupStandardSource s buf 
          return (dev,c,s,ptr,buf)
  where
    errMsg = "ARRGG! Me scurvy computer be not OpenAL friendly!!"
    
cleanUp :: (Device,Context,Source,Ptr Word8,Buffer) -> IO ()
cleanUp (d,c,s,p,b) = do
  free p
  deleteObjectNames [s]
  deleteObjectNames [b]
  destroyContext c
  closeDevice d
  return ()
  
playSound :: ByteString -> IO ()
playSound sound = do
  p@(_,_,s,_,_) <- prepareToPlay sound
  play [s]
  waitForSource s
  cleanUp p 
        
waitForSource :: Source -> IO ()    
waitForSource s = do
  st <- get $ sourceState s
  case st of
    Stopped -> return ()
    _ -> waitForSource s        
