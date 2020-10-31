module Lib.System where

import Lib.Particle
import Lib.SpaceVec

data State = State {
  time :: Double,
  particle :: Particle
}

instance Show State where
  show (State t p) = "t@" ++ show t ++ show p

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
