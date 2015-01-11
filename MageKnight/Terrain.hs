module MageKnight.Terrain where

import MageKnight.Common

import           Data.Map ( Map )
import qualified Data.Map as Map


data Dir            = NE | E | SE | SW | W | NW
                      deriving (Eq,Ord,Show)

data TileLoc        = Center | Border Dir
                      deriving (Eq,Ord,Show)

data TileNeighbour  = Local TileLoc | Foreign Int Int Dir
                      deriving (Eq,Ord,Show)



