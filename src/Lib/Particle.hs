module Lib.Particle where

import Lib.SpaceVec

data Particle = Particle {
  mass :: Double,
  pos :: SpaceVec,
  vel :: SpaceVec
}

instance Show Particle where
  show (Particle mass pos vel) = "p(" ++ show mass ++ ")" ++ show pos ++ show vel

