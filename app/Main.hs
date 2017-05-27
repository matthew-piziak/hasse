{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude             hiding (elements, from, to, width)
import           Diagrams.TwoD.Layout.Grid

import           Data.Foldable

import           Hasse                        (Element, allPosets, rows)

node :: Int -> Diagram B
node n
  = circle 1
    # fc black
    # named n
    # pad 6
    # padX 1.5

row :: [Element] -> Diagram B
row elements = hcat (node <$> elements) # centerX

diagram :: [Element] -> [(Element, Element)] -> Diagram B
diagram elementSet poset = vcat (fmap row rows') # connections # reflectY # padX 1.2
  where rows' = rows poset elementSet
        connections =
          case length poset of
            0 -> id
            _ -> fold $ connection <$> poset
        connection (to, from) = connect' (with & arrowHead .~ noHead & shaftStyle %~ lw 1) to from

it :: Diagram B
it = gridCat (diagram elements <$> allPosets elements)
  where elements = [1, 2, 3, 4, 5]

main :: IO ()
main = mainWith it
