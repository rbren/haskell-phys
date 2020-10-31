module Lib.Systems.MassOnASpring where

import Lib.State
import Lib.SpaceVec
import Lib.Particle
import Lib.Systems.System

massOnASpring :: Double -> Double -> Double -> System
massOnASpring k m x0 = System state a
  where
    p = Particle m (SpaceVec x0 0 0) (SpaceVec 0 0 0)
    a :: AccelerationFunction
    a (State t p) = accel
      where
        x' = x (pos p)
        accelX = -1 * k * x'
        accel = SpaceVec accelX 0 0
    state = State 0 p

