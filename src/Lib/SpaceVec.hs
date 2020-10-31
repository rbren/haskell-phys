module Lib.SpaceVec where

data SpaceVec = SpaceVec {
  x :: Double,
  y :: Double,
  z :: Double
}

instance Show SpaceVec where
  show (SpaceVec x y z) = "<" ++ show x ++ "," ++ show y ++ "," ++ show z ++ ">"

instance Eq SpaceVec where
  (==) (SpaceVec x y z) (SpaceVec x' y' z') = x == x' && y == y' && z == z'

(^+^) :: SpaceVec -> SpaceVec -> SpaceVec
(^+^) (SpaceVec x y z) (SpaceVec x' y' z') = SpaceVec (x+x') (y+y') (z+z')

(^-^) :: SpaceVec -> SpaceVec -> SpaceVec
(^-^) (SpaceVec x y z) (SpaceVec x' y' z') = SpaceVec (x-x') (y-y') (z-z')

(*^) :: Double -> SpaceVec -> SpaceVec
(*^) c (SpaceVec x y z) = SpaceVec (c*x) (c*y) (c*z)

(^*) :: SpaceVec -> Double -> SpaceVec
(^*) (SpaceVec x y z) c = SpaceVec (c*x) (c*y) (c*z)

(^.^) :: SpaceVec -> SpaceVec -> Double
(^.^) (SpaceVec x y z) (SpaceVec x' y' z') = x*x' + y*y' + z*z'

magnitude :: SpaceVec -> Double
magnitude (SpaceVec x y z) = sqrt (x*x + y*y + z*z)

zeroSV = SpaceVec 0 0 0
