module Lib.Systems.System where

import Lib.Particle
import Lib.SpaceVec

type AccelerationFunction = Particle -> SpaceVec

data State = State {
  time :: Double,
  particle :: Particle
}

instance Show State where
  show (State t p) = "t@" ++ show t ++ show p

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
    vel' = vel p ^+^ ((a p) ^* dt)
    p' = Particle (mass p) pos' vel'

rk4Step :: Double -> System -> System
rk4Step   dt (System (State t p) a) = System (State t' p') a
  where
    -- https://math.stackexchange.com/questions/2023819/using-the-runge-kuttas-method-to-solve-a-2nd-derivative-question
    t' = t + dt
    halfDT = dt * 0.5
    tPlus = t + halfDT
    m = mass p

    x0 = pos p
    v0 = vel p
    p0 = Particle m x0 v0
    k0 = a p0

    x1 = x0 ^+^ (halfDT *^ v0)
    v1 = v0 ^+^ (halfDT *^ k0)
    p1 = Particle m x1 v1
    k1 = a p1

    x2 = x0 ^+^ (halfDT *^ v1)
    v2 = v0 ^+^ (halfDT *^ k1)
    p2 = Particle m x2 v2
    k2 = a p2

    x3 = x0 ^+^ (dt *^ v2)
    v3 = v0 ^+^ (dt *^ k2)
    p3 = Particle m x3 v3
    k3 = a p3

    coeff = dt / 6.0
    xDiff = coeff *^ (v0 ^+^ (2 *^ v1) ^+^ (2 *^ v2) ^+^ v3)
    vDiff = coeff *^ (k0 ^+^ (2 *^ k1) ^+^ (2 *^ k2) ^+^ k3)

    x' = x0 ^+^ xDiff
    v' = v0 ^+^ vDiff
    p' = Particle m x' v'
