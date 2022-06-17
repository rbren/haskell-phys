{-# LANGUAGE RankNTypes          #-}

module Lib.HamiltonianSystem where

import Lib.SpaceVec
import Numeric.LinearAlgebra.Static
import qualified Data.Vector.Sized as V
import GHC.TypeLits
import Numeric.AD
import GHC.Generics


-- n general coords, m-dimensional cartesian space
{-
data HamiltonianSystem m n = HamiltonianSystem {
  inertias :: R m,
  getCartesianCoords :: R n -> R m,
  getJacobianForCoords :: R n -> L m n,
  getHessianForCoords :: R n -> V.Vector n (L m n),
  potential :: R n -> Double,
  potentialGradient :: R n -> R n
}
-}

data HamiltonianSystem a m n = HamiltonianSystem {
  inertias :: (RealFloat a, KnownNat m) => V.Vector m a,
  getCartesianCoords ::  (RealFloat a, KnownNat n, KnownNat m) => V.Vector n a -> V.Vector m a,
  potential :: (RealFloat a, KnownNat n) => V.Vector n a -> Double
}

{-
getJacobianForCoords :: HamiltonianSystem a m n -> V.Vector n a -> V.Vector m (V.Vector n a)
getJacobianForCoords s c = (jacobian (getCartesianCoords s)) c

potentialGradient :: HamiltonianSystem a m n -> V.Vector n a -> V.Vector n a
potentialGradient s v = (grad (getCartesianCoords s)) v
-}

{-
data HamiltonianConfiguration n = HamiltonianConfiguration {
  positions :: R n,
  velocities :: R n
} deriving Show

data HamiltonianPhase n = HamiltonianPhase {
  positions :: R n,
  momenta :: R n
} deriving Show
-}
