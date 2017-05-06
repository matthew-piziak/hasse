{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Hasse where

import           Algebra.Lattice
import           Data.Colour.SRGB             (sRGB24read)
import           Data.Enumerable              (Enumerable (..))
import           Data.Function                (on)
import           Data.List
import           Data.Maybe                   (fromMaybe)
import qualified Data.MultiSet                as MS
import           Data.Ord                     (comparing)
import qualified Data.Set                     as S
import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude

-- TODO: turn into records and lenses
-- TODO: add to git
data Poset = Poset [Row] deriving (Show, Eq, Ord)

data Row = Row (MS.MultiSet Element) deriving (Show, Eq, Ord)

data Element = Element { down :: Int, up :: Int } deriving (Show, Eq, Ord)

posets :: Int -> S.Set Poset
posets 0 = S.singleton $ Poset $ []
posets n = undefined

extend :: Poset -> S.Set Poset
extend (Poset [])           = S.singleton $ Poset [Row $ MS.singleton empty]
extend (Poset ((Row r):rs)) = S.singleton $ Poset [Row (MS.union r (MS.singleton empty))]

empty :: Element
empty = Element { down = 0, up = 0 }
