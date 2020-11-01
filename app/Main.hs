{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Lib.Main.SpringField
import Diagrams.Backend.Cairo.CmdLine

main = mainWith gif
