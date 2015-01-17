{-# LANGUAGE RecordWildCards #-}
module MageKnight.GameTile where

import           MageKnight.Common
import           MageKnight.Terrain
import           MageKnight.HexContent
import           MageKnight.Enemies
import           MageKnight.Ruins
import           MageKnight.ResourceQ (ResourceQ)
import qualified MageKnight.ResourceQ as RQ

import           Data.Map ( Map )
import qualified Data.Map as Map


data GameTile = GameTile
  { gameTile        :: Tile
  , gameTileContent :: Map HexAddr HexContent
  }




