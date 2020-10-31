module Lib.Systems.Gravitation where

import Lib.SpaceVec
import Lib.Particle
import Lib.Systems.System

gravConstant = 6.67e-11

gravitation :: Double -> Particle -> System
gravitation mBase p = System state a
  where
    a :: AccelerationFunction
    a p = accel
      where
        dist = magnitude (pos p)
        dir = (pos p) ^* (-1.0 / dist)
        mag = (gravConstant * mBase) / (dist * dist)
        accel = dir ^* mag
    state = State 0 p

