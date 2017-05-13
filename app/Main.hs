{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diagrams.Prelude hiding (elements, width, to, from)
import Diagrams.Backend.SVG.CmdLine

import Protolude (fold)

import Data.List.Split

import Hasse (
  Element
  , allPosets
  , rows
  )

node :: Int -> Diagram B
node n
  = circle 1
    # fc black
    # named n
    # pad 2

row :: [Element] -> Diagram B
row elements = (hcat $ node <$> elements) # centerX

diagram :: [Element] -> [(Element, Element)] -> Diagram B
diagram elementSet poset = (vcat $ (fmap row rows')) # connections # rotateBy (1/2)
  where rows' = rows poset elementSet
        connections =
          case length poset of
            0 -> id
            _ -> fold $ connection <$> poset
        connection (to, from) = connect' (with & arrowHead .~ noHead) to from

it :: Diagram B
it = vcat $ hcat <$> chunksOf 7 (diagram elements <$> (allPosets elements))
  where elements = [1, 2, 3, 4, 5]

main :: IO ()
main = mainWith $ it
