{-# LANGUAGE RecordWildCards #-}
module MageKnight.Terrain where

import MageKnight.Common

import           Data.Map ( Map )
import qualified Data.Map as Map

type TileAddr       = (Int,Int)

data Dir            = NE | E | SE | SW | W | NW
                      deriving (Eq,Ord,Show,Enum,Bounded)

data HexAddr        = Center | Border Dir
                      deriving (Eq,Ord,Show)

data HexNeighbour   = Local HexAddr | Foreign Int Int Dir
                      deriving (Eq,Ord,Show)

data Addr           = Addr { tileAddr :: TileAddr, hexAddr :: HexAddr }
                      deriving (Eq,Show)


neighbour :: Addr -> Dir -> Addr
neighbour Addr { .. } dir =
  case tileGraph hexAddr dir of
    Local x         -> Addr { hexAddr = x, .. }
    Foreign dx dy a -> Addr { tileAddr = (fst tileAddr + dx, snd tileAddr + dy)
                            , hexAddr  = Border a
                            }

oppositeDir :: Dir -> Dir
oppositeDir dir =
  case dir of
    NE -> SW
    E  -> W
    SE -> NW
    SW -> NE
    W  -> E
    NW -> SE

tileGraph :: HexAddr -> Dir -> HexNeighbour
tileGraph hexAddr d =
  case hexAddr of
    Center -> Local (Border d)
    Border b ->
      case b of
        NW -> case d of
                NW -> Foreign 0 1 SW
                NE -> Foreign 0 1 SE
                E  -> Local (Border NE)
                SE -> Local Center
                SW -> Local (Border W)
                W  -> Foreign (-1) 1 E
        NE -> case d of
                NW -> Foreign 0 1 SE
                NE -> Foreign 1 0 W
                E  -> Foreign 1 0 SW
                SE -> Local (Border E)
                SW -> Local Center
                W  -> Local (Border NW)
        E -> case d of
               NW -> Local (Border NE)
               NE -> Foreign 1 0 SW
               E  -> Foreign 1 (-1) NW
               SE -> Foreign 1 (-1) W
               SW -> Local (Border SE)
               W  -> Local Center
        SE -> case d of
                NW -> Local Center
                NE -> Local (Border E)
                E  -> Foreign 1 (-1) W
                SE -> Foreign 0 (-1) NE
                SW -> Foreign 0 (-1) NW
                W  -> Local (Border SW)
        SW -> case d of
                NW -> Local (Border W)
                NE -> Local Center
                E  -> Local (Border SE)
                SE -> Foreign 0 (-1) NW
                SW -> Foreign (-1) 0 E
                W  -> Foreign (-1) 0 NE
        W -> case d of
               NW -> Foreign (-1) 1 E
               NE -> Local (Border NW)
               E  -> Local Center
               SE -> Local (Border SW)
               SW -> Foreign (-1) 0 NE
               W  -> Foreign (-1) 1 SE






