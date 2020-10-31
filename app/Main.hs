{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Data.Typeable
import Lib.SpaceVec
import Lib.Particle
import Lib.State
import Lib.Systems.System
import Lib.Systems.MassOnASpring

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

sys = massOnASpring 90.0 1.0 1.0

delta = 0.01
endTime = 10
startTime = time (state sys)
--steps = toInteger ((endTime - startTime) / delta)
steps = 500

solution = iterate (eulerStep delta) sys
systemSteps = take steps solution

getCircle :: System -> Diagram B
--getCircle sys = circle (((time (state sys)) + 1.0) * 1.0) # lc white <> square 4 # lw none
getCircle sys = circle 0.1 # lc white <> square 4 # lw none # translate (r2 (xLoc, yLoc))
  where
    p = pos (particle (state sys))
    xLoc = x p
    yLoc = y p

--getCircle sys = circle 1 # lc white <> square 4 # lw none

gif :: [(Diagram B, Int)]
gif = map (\x -> (getCircle x, 1)) systemSteps

--main = mapM_ print systemSteps
main = mainWith gif

