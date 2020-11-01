module Lib.SpringField where

import Data.Typeable
import Data.Vector as V
import GHC.Float

import Diagrams.Prelude
import Diagrams.Backend.Cairo.CmdLine

type SpringField = V.Vector (V.Vector Double)

makeSpringField :: [[Double]] -> SpringField
makeSpringField nums = sf
  where
    sf = V.fromList (Prelude.map V.fromList nums)

indexSF :: SpringField -> [[(Int, Int, Double)]]
indexSF sf = sf'
  where
    sfI = V.indexed (V.map V.indexed sf)
    indexCell row (col, val) = (row, col, val)
    indexRow (rowIdx, row) = V.toList (V.map (indexCell rowIdx) row)
    sf' = V.toList (V.map indexRow sfI)

flattenSF :: SpringField -> [(Int, Int, Double)]
flattenSF sf = Prelude.concat (indexSF sf)

evolveSF :: Double -> SpringField -> SpringField
evolveSF dt sf = sf'
  where
    sfI = indexSF sf
    evolveCell (row, col, val) = val
    sf' = makeSpringField (Prelude.map (Prelude.map evolveCell) sfI)


draw :: SpringField -> Diagram B
draw sf = pic
  where
    boxSize = fromIntegral (V.length sf - 1)
    radius = 0.5
    sfI = flattenSF sf
    box = square boxSize # lw 0.5 # lc white

    mkPoint :: Int -> Int -> P2 Double
    mkPoint row col = p2 (fromIntegral row - boxSize / 2, fromIntegral col - boxSize / 2)

    getCircle :: (Int, Int, Double) -> (P2 Double, Diagram B)
    getCircle (row, col, val) = (mkPoint row col, circle radius # lc colr)
      where
        r = Prelude.maximum [-1.0 * val, 0.0]
        g = 0.0
        b = Prelude.maximum [val, 0.0]
        colr = sRGB r g b

    circles = Prelude.map getCircle sfI
    pic = ((position circles) <> box) # bg white
