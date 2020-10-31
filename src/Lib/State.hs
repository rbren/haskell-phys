module Lib.State where

import Lib.Particle

data State = State {
  time :: Double,
  particle :: Particle
}

instance Show State where
  show (State t p) = "t@" ++ show t ++ show p


