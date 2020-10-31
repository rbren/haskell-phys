{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Data.Typeable
import Lib.SpaceVec
import Lib.Particle

import Lib.Systems.System
import Lib.Systems.MassOnASpring

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

sys = massOnASpring 900.0 1.0 1.0

delta = 0.01
endTime = 10
startTime = time (state sys)
--steps = toInteger ((endTime - startTime) / delta)
steps = 100

solution = iterate (rk4Step delta) sys
systemSteps = take steps solution

getCircle :: System -> Diagram B
getCircle sys = circle 0.1 # lc white # translate (r2 (xLoc, yLoc))
  where
    p = pos (particle (state sys))
    xLoc = x p
    yLoc = y p

getLabel :: System -> Diagram B
getLabel sys = text label # fontSizeL 0.1 # fc white # alignBL
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
    bl = p2 (-1.8, -1.8)
    box = square 4 # lw 0.5 # lc white
    getSystemDiagram :: System -> Diagram B
    getSystemDiagram sys = position [(origin, getCircle sys), (bl, getLabel sys)] <> box

main = mainWith gif

