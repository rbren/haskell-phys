module Lib.Main.Spacetime where

import Lib.SpaceVec
import Lib.SpacetimeVec

st = SpacetimeVec 1 (SpaceVec 1 0 0)
v = SpaceVec 0.5 0 0
st' = transformCoordinates st v

mainFuncST = print st'
