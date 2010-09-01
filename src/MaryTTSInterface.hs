module MaryTTSInterface where

import Network.URI
import Network.HTTP
import Data.List
import Data.ByteString (ByteString)
import Embr

type Phoneme = String
type Duration = Double

standardRequest text server port outputType audio =
  "http://" ++ server ++ ":" ++ port ++
  "/process?INPUT_TEXT=" ++ text' ++
  "&INPUT_TYPE=TEXT&OUTPUT_TYPE=" ++ outputType ++
  audio' ++ "&LOCALE=en_US" where
    text' = intercalate "+" $ words text
    audio' = case audio of
      Nothing -> ""
      Just w -> "&AUDIO=" ++ w

getPhonemesDurations :: String -- ^ The text to be synthesized
                        -> String -- ^ The IP of the mary server
                        -> String -- ^ The port at which the server is running
                        -> IO [(Phoneme,Duration)]
getPhonemesDurations text server port = do
  res <- simpleHTTP (getRequest req)
  response <- getResponseBody res
  return $ map collect $ filter (not . comment) $ lines response where
    req = standardRequest text server port "REALISED_DURATIONS" Nothing    
    comment ('#' : _) = True
    comment _ = False
    collect row = (ph,read dur) where
      (dur : _ : ph : _) = words row
  
  

getAudio :: String -- ^ The text to be synthesized
            -> String -- ^ The IP of the MaryTTS server
            -> String -- ^ The port at which the server is listening
            -> IO ByteString
getAudio text server port = do
  res <- simpleHTTP (byteReq req')
  getResponseBody res where
    req = parseURI $ standardRequest text server port "AUDIO" (Just "WAVE_FILE")    
    req' = case req of
      Nothing -> error "No graceful termination. Problem with the URL to get sound from MARY-TTS"
      Just r -> r
    byteReq :: URI -> Request ByteString
    byteReq uri = mkRequest GET uri

standardPort = "59125"
standardServer = "localhost"

type PhonemeVisemeMap = [(Phoneme,Viseme)]
type Viseme = MorphKey

lookup :: Phoneme -> PhonemeVisemeMap -> Viseme
lookup p pvm = case Prelude.lookup p pvm of
  Nothing -> error $ "Pretty bad error handling. In any case we are missing a phoneme->viseme definition for: " ++ p
  Just v -> v
  
generateMorphKeysSequence :: [(Phoneme,Duration)] -- ^ The list of timed phonemes returned by MaryTTS
                             -> PhonemeVisemeMap ->
                             [KPose]
generateMorphKeysSequence timedPhon@((p,_) : _) pvm = first : aux timedPhon where                             
  first = KPoseMorphTarget 0 (Just 0) [(MaryTTSInterface.lookup p pvm,1)]
  aux [(p,d)] = [KPoseMorphTarget (scale d) (Just 0) [(MaryTTSInterface.lookup p pvm,0)]]
  aux ((p1,d1) : pv2@(p2,_) : rest) = [KPoseMorphTarget (scale d1) (Just 0) [(MaryTTSInterface.lookup p1 pvm,0),
                                                                             (MaryTTSInterface.lookup p2 pvm,1)]] ++ 
                                      aux (pv2:rest)
  scale :: Double -> Int
  scale d = truncate $ d * 1000