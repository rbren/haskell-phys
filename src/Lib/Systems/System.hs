module Lib.Systems.System where

import Lib.Particle
import Lib.SpaceVec
import Lib.State

type AccelerationFunction = State -> SpaceVec

data System = System {
  state :: State,
  accel :: AccelerationFunction
}

instance Show System where
  show (System s a) = show s

eulerStep :: Double -> System -> System
eulerStep dt (System (State t p) a) = System (State t' p') a
  where
    t' = t + dt
    pos' = pos p ^+^ (vel p ^* dt)
    vel' = vel p ^+^ (a(State t p) ^* dt)
    p' = Particle (mass p) pos' vel'

