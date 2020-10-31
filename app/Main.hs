{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Lib.Main.Gravitation
import Diagrams.Backend.Cairo.CmdLine

main = mainWith gif
