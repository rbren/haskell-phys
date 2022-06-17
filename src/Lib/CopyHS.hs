{-# LANGUAGE RankNTypes          #-}

module Lib.CopyHS where

import Lib.SpaceVec
import Numeric.LinearAlgebra.Static
import Numeric.LinearAlgebra.Static.Vector
import qualified Data.Vector.Sized as V
import qualified Data.Vector.Generic.Sized           as VG
import GHC.TypeLits
import Numeric.AD
--import GHC.Generics


data System m n = System
    { sysInertia       :: R m                         -- ^ 'm' vector
    , sysCoords        :: R n -> R m                  -- ^ f
    , sysJacobian      :: R n -> L m n                -- ^ J_f
    , sysHessian       :: R n -> V.Vector n (L m n)   -- ^ H_f
    , sysPotential     :: R n -> Double               -- ^ U
    , sysPotentialGrad :: R n -> R n                  -- ^ grad U
    }

data Config n = Config
    { confPositions  :: R n
    , confVelocities :: R n
    }
  deriving Show

data Phase n = Phase
    { phasePositions :: R n
    , phaseMomenta   :: R n
    }
  deriving Show

underlyingPosition
    :: System m n
    -> R n
    -> R m
underlyingPosition = sysCoords

momenta
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Config n
    -> R n
momenta s (Config q v) = tr j #> mHat #> j #> v
  where
    j    = sysJacobian s q
    mHat = diag (sysInertia s)

toPhase
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Config n
    -> Phase n
toPhase s c = Phase (confPositions c) (momenta s c)

tr2 :: (KnownNat m, KnownNat n)
    => V.Vector m (L n n)
    -> V.Vector n (L m n)
tr2 = fmap rowsL . traverse lRows

vec2l
    :: (KnownNat m, KnownNat n)
    => V.Vector m (V.Vector n Double)
    -> L m n
vec2l = rowsL . fmap (vecR . VG.convert)


mkSystem
    :: (KnownNat m, KnownNat n)
    => R m
    -> (forall a. RealFloat a => V.Vector n a -> V.Vector m a)
    -> (forall a. RealFloat a => V.Vector n a -> a)
    -> System m n
mkSystem m f u = System
                    -- < convert from      | actual thing | convert to >
    { sysInertia       =                     m
    , sysCoords        = vecR . cFrom      . f            . cTo . rVec
    , sysJacobian      = tr   . vec2l      . jacobianT f  . cTo . rVec
    , sysHessian       = tr2  . fmap vec2l . hessianF f   . cTo . rVec
    , sysPotential     =                     u            . cTo . rVec
    , sysPotentialGrad = vecR . cFrom      . grad u       . cTo . rVec
    }
  where
    cTo   = VG.convert
    cFrom = VG.convert

hamilEqns
    :: (KnownNat n, KnownNat m)
    => System m n
    -> Phase n
    -> (R n, R n)       -- dq/dt and dp/dt
hamilEqns s (Phase q p) = (dqdt, dpdt)
  where
    j       = sysJacobian s q
    trj     = tr j
    mHat    = diag (sysInertia s)
    kHat    = trj `mul` mHat `mul` j
    kHatInv = inv kHat
    dqdt    = kHatInv #> p
    dpdt    = vecR (VG.convert bigUglyThing) - sysPotentialGrad s q
      where
        bigUglyThing =
          fmap (\j2 -> -p <.> kHatInv #> trj #> mHat #> j2 #> kHatInv #> p)
               (sysHessian s q)

stepEuler
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ q(t) and p(t)
    -> Phase n          -- ^ q(t + dt) and p(t + dt)
stepEuler s dt ph@(Phase q p) = Phase (q + konst dt * dq) (p + konst dt * dp)
  where
    (dq, dp) = hamilEqns s ph

runSystem
    :: (KnownNat n, KnownNat m)
    => System m n       -- ^ the system
    -> Double           -- ^ dt
    -> Phase n          -- ^ initial phase
    -> [Phase n]        -- ^ progression of the system using Euler integration
runSystem s dt = go
  where
    go p0 = p0 : go (stepEuler s dt p0)
