import SimpleSound
import MaryTTSInterface
import Embr
import EmbrSocket
import System.Posix.Process hiding (Stopped)
import System.Posix.Types
import Sound.OpenAL
import Paths_Embr
import Data.Monoid
import Gesture

text = "welcome to the world of speech synthesis" 

say text = do
  sound <- getAudio text standardServer standardPort
  playSound sound

test = do
  mapFile <- return "/home/gianluca/Dropbox/BD/code/embr/data/phoneme-viseme.map" -- getDataFileName "phoneme-viseme.map"
  m <- readFile mapFile
  ps <- getPhonemesDurations text standardServer standardPort
  sound <- getAudio text standardServer standardPort
  p@(_,_,s,_,_) <- prepareToPlay sound
  cmd <- return $  Sequence KPoseSequenceListOfPoses { animatedCharacter = "Amber"
                                                     , startTime = 1000
                                                     , fadeIn = Nothing
                                                     , fadeOut = Nothing
                                                     , timeWarp = Nothing
                                                     , poses = generateMorphKeysSequence ps (read m)}
  print cmd
  embr <- forkProcess $ standardAct (TimeReset `mappend` cmd)
  play [s]
  waitForSource s
  cleanUp p 
  waitForProcess embr "embr"
--  waitForProcess audio "audio"
  
stdKPoseSequence = KPoseSequenceListOfPoses "Amber" 0 Nothing Nothing Nothing []

square = do
  c <- return (TimeReset `mappend` cmd)
  print c
  standardAct c where
  cmd = Sequence $ stdKPoseSequence{poses = ps}
--  ps = [rest]
  ps = traceTrajectory LHand LArm [] 1000
  points = [Vector3D (-0.5) 0 (-0.5),
            Vector3D 0.5 0 (-0.5),
            Vector3D 0.5 0 0.5,
            Vector3D (-0.5) 0 0.5]

--waitForProcess :: ProcessID -> IO ()
waitForProcess pid name = do
  res <- getProcessStatus True True pid
  case res of
    Just _ -> putStrLn $ "Process " ++ name ++ " has terminated."
    _ -> waitForProcess pid name
    
main = square