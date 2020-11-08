module Lib.SpacetimeVec where

import Lib.SpaceVec

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
($.$) (SpacetimeVec t r) (SpacetimeVec t' r') = -t*t' + r^.^r'

magnitude :: SpacetimeVec -> Double
magnitude (SpacetimeVec t r) = sqrt (t*t + a*a + b*b + c*c)
  where
   a = x r
   b = y r
   c = z r

zeroSTV = SpacetimeVec 0 (SpaceVec 0 0 0)
