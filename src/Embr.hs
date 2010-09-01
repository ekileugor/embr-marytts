module Embr where

import Data.Monoid hiding (All)

data WarpType = TAN | EXP deriving Show

data BodyGroup = All
               | Head
               | HeadNeck
               | HeadThorax
               | HeadAbdomen
               | UpperBodyNoArms
               | LHand
               | RHand
               | LArm
               | RArm deriving (Enum,Bounded)

instance Show BodyGroup where
  show All = "all"
  show Head = "head"
  show HeadNeck = "headNeck"
  show HeadThorax = "headThorax"
  show HeadAbdomen = "headAbdomen"
  show UpperBodyNoArms = "upperBodyNoArms"
  show LHand = "lhand"
  show RHand = "rhand"
  show LArm = "larm"
  show RArm = "rarm"

data PoseKey = HandsClaw
             | HandsFist
             | HandsIndex
             | HandsOpenRelaxed
             | HandsOpenSpread
             | HandsOpenStraight
             | HandsPurse
             | HandsRing
             | HandsThumbUp deriving (Enum,Bounded)
               
instance Show PoseKey where
  show HandsClaw = "hands_claw"
  show HandsFist = "hands_fist"
  show HandsIndex = "hands_index"
  show HandsOpenRelaxed = "hands_open-relaxed"
  show HandsOpenSpread = "hands_open-spread"
  show HandsOpenStraight = "hands_open-straight"
  show HandsPurse = "hands_purse"
  show HandsRing = "hands_ring"
  show HandsThumbUp = "hands_thumbUp"

data MorphKey = ExpSmileClosed
              | ExpAnger
              | ExpDisgust
              | ExpFear
              | ExpSad
              | ExpSurprise
              | ExpSmileOpen
              | ModBlinkLeft
              | ModBlinkRight
              | ModBrowDownLeft
              | ModBrowDownRight
              | ModBrowInRight
              | ModBrowInLeft
              | ModBrowUpLeft
              | ModBrowUpRight
              | ModLookDown
              | ModLookLeft
              | ModLookRight
              | ModLookUp
              | PhonAah
              | PhonBMP
              | PhonBigaah
              | PhonChJSh
              | PhonDST
              | PhonEe
              | PhonEh
              | PhonFV
              | PhonI
              | PhonK
              | PhonN
              | PhonOh
              | PhonOohQ
              | PhonR
              | PhonTh
              | PhonW deriving (Enum,Bounded,Read)

instance Show MorphKey where
  show ExpSmileClosed = "ExpSmileClosed"
  show ExpAnger = "ExpAnger"
  show ExpDisgust = "ExpDisgust"
  show ExpFear = "ExpFear"
  show ExpSad = "ExpSad"
  show ExpSurprise = "ExpSurprise"
  show ExpSmileOpen = "ExpSmileOpen"
  show ModBlinkLeft = "ModBlinkLeft"
  show ModBlinkRight = "ModBlinkRight"
  show ModBrowDownLeft = "ModBrowDownLeft"
  show ModBrowDownRight = "ModBrowDownRight"
  show ModBrowInRight = "ModBrowInRight"
  show ModBrowInLeft = "ModBrowInLeft"
  show ModBrowUpLeft = "ModBrowInLeft"
  show ModBrowUpRight = "ModBrowUpRight"
  show ModLookDown = "ModLookDown"
  show ModLookLeft = "ModLookLeft"
  show ModLookRight = "ModLookRight"
  show ModLookUp = "ModLookUp"
  show PhonAah = "Phonaah"
  show PhonBMP = "PhonB,M,P"
  show PhonBigaah = "Phonbigaah"
  show PhonChJSh = "Phonch,J,sh"
  show PhonDST = "PhonD,S,T"
  show PhonEe = "Phonee"
  show PhonEh = "Phoneh"
  show PhonFV = "PhonF,V"
  show PhonI = "Phoni"
  show PhonK = "PhonK"
  show PhonN = "PhonN"
  show PhonOh = "Phonoh"
  show PhonOohQ = "Phonooh,Q"
  show PhonR = "PhonR"
  show PhonTh = "PhonR"
  show PhonW = "PhonW"
  

data Axis = XAxis
          | YAxis
          | ZAxis
         

instance Show Axis where
  show XAxis = "XAxis"
  show YAxis = "YAxis"
  show ZAxis = "ZAxis"

data Vector3D = Vector3D Double Double Double

instance Show Vector3D where
  show (Vector3D x y z) = show x ++ ";" ++ show y ++ ";" ++ show z


data KPoseSequence = 
  KPoseSequencePreRecorded { 
    -- | Tha name of the character. Not much useful if not Amber
    animatedCharacter :: String
    -- | The start time for the animtation. Relative to the last call to TIME-RESET. Specified in milliseconds
    , startTime :: Int
    -- | Linear ramp defining a progressive appearance of the animation. Specified in milliseconds
    , fadeIn :: Maybe Int
    -- | Linear ramp defining a progressive disappearance of the animation. Specified in milliseconds
    , fadeOut :: Maybe Int
    -- | Mhm...
    , timeWarp :: Maybe (WarpType,Double)
    -- | Desired duration for the recorded animation in milliseconds. The pre-recorded animation will be shrunk or extended to fit the 'animationTimePoint' requirement.  
    , animationTimePoint :: Int
    -- | Unique identifier for the pre-recorded animation
    , animationClip :: String
    -- | The body group???
    , bodyGroup :: BodyGroup}
  | KPoseSequenceListOfPoses {
    -- | Tha name of the character. Not much useful if not Amber
    animatedCharacter :: String
    -- | The start time for the animtation. Relative to the last call to TIME-RESET. Specified in milliseconds
    , startTime :: Int
    -- | Linear ramp defining a progressive appearance of the animation. Specified in milliseconds
    , fadeIn :: Maybe Int
    -- | Linear ramp defining a progressive disappearance of the animation. Specified in milliseconds
    , fadeOut :: Maybe Int
    -- | Mhm...
    , timeWarp :: Maybe (WarpType,Double)
    -- | The list of poses
    , poses :: [KPose]
    }
    
maybeToString :: Maybe String -> String    
maybeToString Nothing = ""
maybeToString (Just a) = a

poseSeqBoiler ac st fi fo tw body =     
  "BEGIN K_POSE_SEQUENCE\nCHARACTER:" ++
  ac ++ "\nSTART:" ++
  show st ++ "\n" ++
  maybeToString (fi >>= \fi -> Just ("FADE_IN:" ++
                                     show fi ++ "\n")) ++
  maybeToString (fo >>= \fo -> Just ("FADE_OUT:" ++
                                     show fo ++ "\n")) ++
  maybeToString (tw >>= \(f,v) -> Just ("TIME_WARP:" ++
                                        show f ++ ";" ++
                                        show v ++ "\n")) ++
  body ++
  "END\n"
    
instance Show KPoseSequence where
  show (KPoseSequenceListOfPoses ac st fi fo tw ps) =
    poseSeqBoiler ac st fi fo tw $
    unlines $ map show ps
  show (KPoseSequencePreRecorded ac st fi fo tw atp acl bg) =
    poseSeqBoiler ac st fi fo tw $
    "BEGIN ANIMATION\nTIME_POINT:" ++
    show atp ++ "\nANIMATION_CLIP:" ++
    show acl ++ "\nBODY_GROUP:" ++
    show bg ++ "\nEND\n"
    
data KPose = 
  KPosePreRecorded {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The body group of the pose
    , poseBodyGroup :: BodyGroup
    -- | Indentifier for pre-recorded pose
    , poseKey :: PoseKey
    }
  | KPosePositionConstraint {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The body group of the pose
    , poseBodyGroup :: BodyGroup
    -- | Target x,y,z in agent local frame (expressed in meters) (I wonder how tall Amber is...)
    , target :: Vector3D  
    -- | Offset ...  
    , offset :: Vector3D
    -- | The joint that performs the movement  
    , joint :: BodyGroup -- Here we have an interesting problem: the 
                         -- value of 'joint' is dependent on the value
                         -- of poseBodyGroup. I wonder if it is possible to express
                         -- this requirement at the type level somehow...       
    }
  | KPoseOrientationConstraint {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The body group of the pose    
    , poseBodyGroup :: BodyGroup
    -- | which local axis of the joint should be aligned with 'direction'  
    , normal :: Axis
    -- | Direction of the orientation constraint  
    , direction :: Vector3D
    -- | The joint that performs the movement  
    , joint :: BodyGroup
    }
  | KPoseSwivelConstraint {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The body group of the pose
    , poseBodyGroup :: BodyGroup
    -- | The angle of the swivel
    , swivel :: Double
    }
  | KPoseLookAtConstraint {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The body group of the pose. The only allowed values are 'Head', 'HeadNeck', 'HeadThorax', 'HeadAbdomen'
    , poseBodyGroup :: BodyGroup
    -- | Target point  
    , target :: Vector3D
    }
  | KPoseMorphTarget {
    -- | The time point of the pose inside the sequence  
    timePoint :: Int
    -- | Hold...
    , hold :: Maybe Int
    -- | The morphs to applied combined with the respective weights - weights should be from 0 to 1
    , morphs :: [(MorphKey,Double)]
    }
    
poseBoiler tp h body = 
  "BEGIN K_POSE\nTIME_POINT:+" ++
  show tp ++ "\n" ++
  maybeToString (h >>= \h -> Just ("HOLD:" ++ show h ++ "\n")) ++
  body ++ "END"

instance Show KPose where
  show (KPoseMorphTarget tp h ms) =
    poseBoiler tp h $
    ms' where
      ms' = unlines $ map f ms
      f (mk,mv) =     
        "BEGIN MORPH_TARGET\nMORPH_KEY:" ++
        show mk ++ "\n" ++
        "MORPH_VALUE:" ++ show mv ++ "\nEND"
  show (KPoseLookAtConstraint tp h pbg t) =
    poseBoiler tp h $
    "BEGIN LOOK_AT_CONSTRAINT\nBODY_GROUP:" ++
    show pbg ++ "\nTARGET:" ++
    show t ++ "\nEND\n"
  show (KPoseSwivelConstraint tp h pbg s) =
    poseBoiler tp h $
    "BEGIN SWIVEL_CONSTRAINT\nBODY_GROUP:" ++
    show pbg ++ "\nSWIVEL_ANGLE:" ++
    show s ++ "\nEND\n"
  show (KPoseOrientationConstraint tp h pbg n d j) =
    poseBoiler tp h $
    "BEGIN ORIENTATION_CONSTRAINT\nBODY_GROUP:" ++
    show pbg ++ "\nNORMAL:" ++
    show n ++ "\nDIRECTION:" ++
    show d ++ "\nJOINT:" ++
    show j ++ "\nEND\n"
  show (KPosePositionConstraint tp h pbg t o j) = 
    poseBoiler tp h $
    "BEGIN POSITION_CONSTRAINT\nBODY_GROUP:" ++
    show pbg ++ "\nTARGET:" ++
    show t ++ "\nJOINT:" ++
    show j ++ "\nOFFSET:" ++
    show o ++ "\nEND\n"
  show (KPosePreRecorded tp h pbg pk) =
    poseBoiler tp h $
    "BEGIN POSE_TARGET\nBODY_GROUP:" ++
    show pbg ++ "\nPOSE_KEY:" ++
    show pk ++ "\nEND\n"
    
    
data Command = TimeReset
             | Sequence KPoseSequence
             | CmdCons Command Command
             | Empty
               
instance Show Command where               
  show TimeReset = "TIME_RESET"
  show (Sequence s) = show s
  show (CmdCons s z) = show s ++ "\n" ++ show z
  show Empty = ""
               
instance Monoid Command where               
  mempty = Empty
  mappend a b = CmdCons a b



