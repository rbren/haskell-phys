module Lib.NaturalUnits where

import Lib.SpaceVec
import Lib.SpacetimeVec
-- https://www.seas.upenn.edu/~amyers/NaturalUnits.pdf

-- set all these == 1
speedOfLight = 2.9979e8 -- m/s
hBar = 1.0546e-34 -- Joule-seconds
epsilon0 = 8.8542e-12 -- A^2 s^4 kg^-1 m^-3
boltzmann = 1.3806e-23 -- J/K

type Nu2siFactor = Double
nu2si :: Nu2siFactor -> Double -> Double
nu2si factor nu = nu * factor
si2nu :: Nu2siFactor -> Double -> Double
si2nu factor nu = nu / factor

nu2siDistanceFactor :: Nu2siFactor
nu2siDistanceFactor = 1.9733e-16
nu2siDistance = nu2si nu2siDistanceFactor
si2nuDistance = si2nu nu2siDistanceFactor

nu2siTimeFactor :: Nu2siFactor
nu2siTimeFactor = 6.5823e-25
nu2siTime = nu2si nu2siTimeFactor
si2nuTime = si2nu nu2siTimeFactor

nu2siSpeedFactor :: Nu2siFactor
nu2siSpeedFactor = 2.9979e8
nu2siSpeed = nu2si nu2siSpeedFactor
si2nuSpeed = si2nu nu2siSpeedFactor

nu2siSpace :: SpaceVec -> SpaceVec
nu2siSpace s = s ^* nu2siDistanceFactor
si2nuSpace :: SpaceVec -> SpaceVec
si2nuSpace s = s ^* (1.0 / nu2siDistanceFactor)

nu2siVelocity :: SpaceVec -> SpaceVec
nu2siVelocity s = s ^* nu2siSpeedFactor
si2nuVelocity :: SpaceVec -> SpaceVec
si2nuVelocity s = s ^* (1.0 / nu2siSpeedFactor)

nu2siSpacetime :: SpacetimeVec -> SpacetimeVec
nu2siSpacetime (SpacetimeVec t r) = SpacetimeVec t' r'
  where
    t' = nu2siTime t
    r' = nu2siSpace r
si2nuSpacetime :: SpacetimeVec -> SpacetimeVec
si2nuSpacetime (SpacetimeVec t r) = SpacetimeVec t' r'
  where
    t' = si2nuTime t
    r' = si2nuSpace r
