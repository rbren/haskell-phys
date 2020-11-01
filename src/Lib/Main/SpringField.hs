module Lib.Main.SpringField where

import Lib.SpringField

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

field = makeSpringField [[0.5, 0.5, 0.5], [0.5, 0.5, 0.5], [0.5, 0.5, 0.5]]
gif :: [(Diagram B, Int)]
gif = [(draw field, 0)]
