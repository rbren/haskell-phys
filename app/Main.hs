module Main where

import Data.Typeable
import Lib.SpaceVec
import Lib.System

sys = massOnASpring 9.0 1.0 1.0

delta = 0.01
endTime = 10
startTime = time (state sys)
--steps = toInteger ((endTime - startTime) / delta)
steps = 500

solution = iterate (eulerStep delta) sys
systemSteps = take steps solution

main = mapM_ print systemSteps
