module Lib.Main.Gravitation where

import Data.Typeable
import Lib.SpaceVec
import Lib.Particle

import Lib.Systems.System
import Lib.Systems.MassOnASpring
import Lib.Systems.Gravitation

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

-- sys = massOnASpring 900.0 1.0 1.0

massOfEarth = 5.98e24
massOfMoon = 7.348e22
earthMoonDist = 384.4e6
moonVelocity = 1.022e3
moon = Particle massOfMoon (SpaceVec earthMoonDist 0 0) (SpaceVec 0 moonVelocity 0)
sys = gravitation massOfEarth moon

boxSize = earthMoonDist * 3
circleSize = boxSize / 50

delta = 1000 * 30
endTime = 10
startTime = time (state sys)
--steps = toInteger ((endTime - startTime) / delta)
steps = 100

solution = iterate (rk4Step delta) sys
systemSteps = take steps solution

getCircle :: System -> Diagram B
getCircle sys = circle circleSize # lc white # translate (r2 (xLoc, yLoc))
  where
    p = pos (particle (state sys))
    xLoc = x p
    yLoc = y p

getLabel :: System -> Diagram B
getLabel sys = text label # fontSizeL circleSize # fc white # alignBL
  where
    t = time (state sys)
    bump = t * 1000
    rounded = fromIntegral (round bump :: Int)
    unbump = rounded / 1000.0
    label = "t=" ++ show unbump

gif :: [(Diagram B, Int)]
gif = map (\sys -> (getSystemDiagram sys, 0)) systemSteps
  where
    origin = p2 (0.0, 0.0)
    labelPos = -0.9 * boxSize / 2.0
    bl = p2 (labelPos, labelPos)
    box = square boxSize # lw 0.5 # lc white
    getSystemDiagram :: System -> Diagram B
    getSystemDiagram sys = position [(origin, getCircle sys), (bl, getLabel sys)] <> box
