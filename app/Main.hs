{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Hasse (allPosets)

node :: Int -> V2 n -> Diagram B
node n pos = circle 1
             # fc black
             # named n
             # pad 2

main = mainWith $ node 0 0
