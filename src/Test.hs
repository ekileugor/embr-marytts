import SimpleSound
import MaryTTSInterface
import Embr
import EmbrSocket
import Threading
import Sound.OpenAL
import Paths_Embr
import Data.Monoid
import Gesture
import Control.Concurrent

text = "The chief of them is the Buck Stone. so called perhaps from the deer which sheltered beneath it, or else from its fancied resemblance to that animal when viewed from certain distant spots." 

say text = do
  sound <- getAudio text standardServer standardPort
  playSound sound

test = do
  mapFile <- return "./data/phoneme-viseme.map" -- getDataFileName "phoneme-viseme.map"
  m <- readFile mapFile
  ps <- getPhonemesDurations text standardServer standardPort
  sound <- getAudio text standardServer standardPort
  p@(_,_,s,_,_) <- prepareToPlay sound
  cmd <- return $  Sequence KPoseSequenceListOfPoses { animatedCharacter = "Amber"
                                                     , startTime = 0
                                                     , fadeIn = Nothing
                                                     , fadeOut = Nothing
                                                     , timeWarp = Nothing
                                                     , poses = generateMorphKeysSequence ps (read m)}
  print cmd
  executeInParallel [standardAct (TimeReset `mappend` cmd), waitMillis 600 >> play [s] >> waitForSource s >> cleanUp p]
  


stdKPoseSequence = KPoseSequenceListOfPoses "Amber" 0 Nothing Nothing Nothing []

squareGesture = Sequence $ stdKPoseSequence{poses = ps} where
  ps = traceTrajectory LHand LArm points 4000
  points = [Vector3D (-0.5) (-0.2) (-0.5),
            Vector3D 0.5 (-0.2) (-0.5),
            Vector3D 0.5 (-0.2) 0.5,
            Vector3D (-0.5) (-0.2) 0.5]
           
-- | 'synchronize text animations delayAnimation delaySpeech' takes care of scheduling the animations and the sound playback, together with generating the lips animation. 'delayAnimation' and 'delaySpeech' are two extimates of the delay that needs to be applied to have a synchronized output
synchronize :: String -- ^ the text to be spoken
               -> [Command] -- ^ the animations that go together with the text
               -> Integer -- ^ the delay to be applied to the animation
               -> Integer -- ^ the delay to be applied to the speech
               -> IO ()
synchronize text animations delayAnimation delaySpeech = do
  mapFile <- return "../data/phoneme-viseme.map" -- getDataFileName "phoneme-viseme.map"
  m <- readFile mapFile
  ps <- getPhonemesDurations text standardServer standardPort
  sound <- getAudio text standardServer standardPort
  p@(_,_,s,_,_) <- prepareToPlay sound
  cmd <- return $  Sequence KPoseSequenceListOfPoses { animatedCharacter = "Amber"
                                                     , startTime = 0
                                                     , fadeIn = Nothing
                                                     , fadeOut = Nothing
                                                     , timeWarp = Nothing
                                                     , poses = generateMorphKeysSequence ps (read m)}
  executeInParallel [waitMillis delayAnimation >> 
                     standardAct (TimeReset `mappend` cmd `mappend` (mconcat animations))
                    , waitMillis delaySound >> play [s] >> waitForSource s >> cleanUp p]
  
  

prerec :: [KPose]
prerec = map (\(p,t) -> KPosePreRecorded (t*1000) (Just 1000) RHand p) $ zip (enumFromTo minBound maxBound) [1..]

main = standardAct $ TimeReset `mappend` (Sequence $ stdKPoseSequence{poses = prerec})