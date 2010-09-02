module Gesture where

import Embr

origin = Vector3D 0 0 0

traceTrajectory :: BodyGroup -- ^ The hand that traces the trajectory
                   -> BodyGroup -- ^ The parent arm
                   -> [Vector3D] -- ^ The trajectory expressed as a list of vertices
                   -> Int -- ^ The number of seconds it takes to perform the gesture
                   -> [KPose] -- ^ a list of key poses
traceTrajectory hand arm vs time = first : points where
  first = KPosePreRecorded 0 (Just 0) hand HandsIndex
  points = map f $ zip vs [1..]
  delta = time `div` (length vs)
  f (v,i) = KPosePositionConstraint (i*delta) (Just 0) arm v origin hand
  
  
rest :: [KPose]  
rest = [KPosePositionConstraint 1500 (Just 0) LArm (Vector3D 0.1 0.3 (-0.4)) origin LHand
       ,KPosePositionConstraint 1500 (Just 0) RArm (Vector3D (-0.1) 0.3 (-0.4)) origin RHand]