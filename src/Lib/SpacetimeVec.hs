module Lib.SpacetimeVec where

import Debug.Trace

import Lib.SpaceVec

speedOfLight = 3e8
speedOfLightSquared = speedOfLight * speedOfLight

data SpacetimeVec = SpacetimeVec {
  t :: Double,
  r :: SpaceVec
}

instance Show SpacetimeVec where
  show (SpacetimeVec t r) = "<" ++ show t ++ "," ++ show (x r) ++ "," ++ show (y r) ++ "," ++ show (z r) ++ ">"

instance Eq SpacetimeVec where
  (==) (SpacetimeVec t r) (SpacetimeVec t' r') = t == t' && r == r'

($+$) :: SpacetimeVec -> SpacetimeVec -> SpacetimeVec
($+$) (SpacetimeVec t r) (SpacetimeVec t' r') = SpacetimeVec (t+t') (r^+^r')

($-$) :: SpacetimeVec -> SpacetimeVec -> SpacetimeVec
($-$) (SpacetimeVec t r) (SpacetimeVec t' r') = SpacetimeVec (t-t') (r^-^r')

(*$) :: Double -> SpacetimeVec -> SpacetimeVec
(*$) c (SpacetimeVec t r) = SpacetimeVec (c*t) (c*^r)

($*) :: SpacetimeVec -> Double -> SpacetimeVec
($*) (SpacetimeVec t r) c = SpacetimeVec (c*t) (c*^r)

($.$) :: SpacetimeVec -> SpacetimeVec -> Double
($.$) (SpacetimeVec t r) (SpacetimeVec t' r') = -t*t'*speedOfLightSquared + r^.^r'

zeroSTV = SpacetimeVec 0 (SpaceVec 0 0 0)

transformCoordinates :: SpacetimeVec -> SpaceVec -> SpacetimeVec
transformCoordinates (SpacetimeVec t r) velocity = traceShow("gamma", gamma) $ SpacetimeVec t' r'
  where
    -- https://en.wikipedia.org/wiki/Lorentz_transformation#Vector_transformations
    speed = magnitude velocity
    direction = velocity ^* (1.0 / speed)
    beta = (speed * speed) / speedOfLightSquared
    gamma = 1.0 / (sqrt (1.0 - beta))
    rParallel = (r ^.^ direction) *^ direction
    rPerpendicular = r ^-^ rParallel
    rParallel' = gamma *^ (rParallel ^-^ (velocity ^* t))
    rPerpendicular' = rPerpendicular
    r' = rParallel' ^+^ rPerpendicular'
    t' = gamma * (t - (velocity ^.^ r) / speedOfLightSquared)
    {-
    speed = (x velocity)
    x0 = (x r)
    t' = gamma * (t - (speed * x0) / speedOfLightSquared)
    x' = gamma * (x0 - speed * t)
    r' = SpaceVec x' 0 0
    -}

findColocated :: SpacetimeVec -> SpaceVec
findColocated (SpacetimeVec t r) = v
  where
    v = r ^* (1/t)
