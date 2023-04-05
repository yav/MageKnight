-- | An active game tile.
module GameTile
  ( GameTile
  , gameTilePlaceHolder
  , emptyGameTile
  , HexInfo(..)
  , gameTileInfo
  , gameTileUpdateAt
  , gameTileUpdateAt'
  , gameTileSearch
  , gameTileIsSafe
  ) where


import           Data.Map ( Map )
import qualified Data.Map as Map

import Common.Basics(PlayerId)
import Terrain
import HexContent

-- | An active game tile.  Keeps track of what's on each hex.
data GameTile = GameTile
  { gameTile        :: Tile
  , gameTileContent :: Map HexAddr HexContent
    -- ^ A missing entry, and `emptyHexContent` should be equivalent.
  }

-- | A place-holder tile.  This is used to indicate where we can explore next.
gameTilePlaceHolder :: TileType -> GameTile
gameTilePlaceHolder t = GameTile { gameTileContent = Map.empty
                                 , gameTile        = placeHolderTile t
                                 }

emptyGameTile :: Tile -> GameTile
emptyGameTile gameTile = GameTile { gameTileContent = Map.empty, .. }


-- | Information about a single hex on an active tile.
data HexInfo = HexInfo
  { hexLandInfo :: HexLandInfo
  , hexContent  :: HexContent     -- ^ Dynamic information
  }

-- | Information about the content of a specific tile.
gameTileInfo :: HexAddr -> GameTile -> HexInfo
gameTileInfo a GameTile { .. } =
  HexInfo { hexContent  = Map.findWithDefault hexEmpty a gameTileContent
          , hexLandInfo = tileTerrain gameTile a
          }

-- | Update the content of a hex.
gameTileUpdateAt :: HexAddr -> (HexInfo -> HexContent) -> GameTile -> GameTile
gameTileUpdateAt a f gt =
  gt { gameTileContent = Map.insert a (f (gameTileInfo a gt))
                                      (gameTileContent gt) }

-- | Update the content of a hex.
gameTileUpdateAt' :: HexAddr -> (HexInfo -> (a,HexContent))
                             -> GameTile -> (a, GameTile)
gameTileUpdateAt' a f gt =
  (res, gt { gameTileContent = Map.insert a content (gameTileContent gt) })
  where (res,content) = f (gameTileInfo a gt)



-- | Find addresses on the tile satisfying a predicate.
gameTileSearch :: (HexInfo -> Bool) -> GameTile -> [ HexAddr ]
gameTileSearch p GameTile { .. } =
  [ a | (a, hexContent) <- Map.toList gameTileContent
      , p HexInfo { hexLandInfo = tileTerrain gameTile a, .. }
      ]


-- | Is this a safe location for the given player.
gameTileIsSafe :: GameTile -> HexAddr -> PlayerId -> Bool
gameTileIsSafe gt loc p =
  case hexTerrain of
    Lake      -> False
    Mountain  -> False
    Ocean     -> False
    _ -> case hexFeature of
           Nothing -> True
           Just f  -> not (hexHasPlayers hexContent) &&
             (case f of
                Keep             -> hexHasShield p hexContent
                MageTower        -> noEnemies
                RampagingEnemy _ -> noEnemies
                City _    -> noEnemies
                _                -> True)
  where
  HexInfo { hexLandInfo = HexLandInfo { .. },  .. } = gameTileInfo loc gt
  noEnemies      = not (hexHasEnemies hexContent)

-- | Would (normal) moving on this tile end the movement phase?
-- XXX: does not aacount for provoking
gameTileEndsMovement :: GameTile -> HexAddr -> PlayerId -> Bool
gameTileEndsMovement gt loc p =
  case hexTerrain of
    _ -> case hexFeature of
           Just MageTower          -> enemies
           Just Keep               -> enemies || not (hexHasShield p hexContent)
           Just (RampagingEnemy _) -> enemies
           Just (City _)           -> enemies
           _                       -> False

  where HexInfo { hexLandInfo = HexLandInfo { .. }, .. } = gameTileInfo loc gt
        enemies        = hexHasEnemies hexContent

-- | Can we walk onto this hex at all?
-- XXX: Not quite right;
--      during normal walking we can't go into mountains
--      using special effects we can go onto rampaging enemies (e.g., flying)
gameTileIsWalkable :: GameTile -> HexAddr -> Bool
gameTileIsWalkable gt loc =
  case hexTerrain of
    Ocean -> False
    _ -> case hexFeature of
           Just (RampagingEnemy _) -> not (hexHasEnemies hexContent)
           _                       -> True

  where HexInfo { hexLandInfo = HexLandInfo { .. }, .. } = gameTileInfo loc gt

