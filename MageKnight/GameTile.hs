{-# LANGUAGE RecordWildCards #-}
module MageKnight.GameTile where

import           MageKnight.Terrain
import           MageKnight.HexContent

import           Data.Map ( Map )
import qualified Data.Map as Map


data GameTile = GameTile
  { gameTile        :: Tile
  , gameTileContent :: Map HexAddr HexContent
  }

gameTileUpdateAt :: HexAddr ->
                    (Terrain -> Maybe Feature -> HexContent -> HexContent) ->
                    GameTile -> GameTile
gameTileUpdateAt a f GameTile { .. } =
  GameTile { gameTileContent = Map.adjust g a gameTileContent, .. }
    where g = case tileTerrain gameTile a of
                (x,y) -> f x y


