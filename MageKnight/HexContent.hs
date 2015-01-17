
{-# LANGUAGE RecordWildCards #-}
module MageKnight.HexContent where

import MageKnight.Bag
import MageKnight.Common
import MageKnight.Enemies
import MageKnight.Ruins

import Data.Maybe (fromMaybe)

data HexContent = HexContent
  { hexShields  :: Bag PlayerId
  , hexEnemies  :: Bag (Visibility, Enemy)
  , hexRuins    :: Maybe (Visibility, Ruins)
  }

hexEmpty :: HexContent
hexEmpty = HexContent
  { hexShields = bagEmpty
  , hexEnemies = bagEmpty
  , hexRuins   = Nothing
  }

hexAddShield :: PlayerId -> HexContent -> HexContent
hexAddShield s HexContent { .. } =
  HexContent { hexShields = bagAdd 1 s hexShields, .. }

hexReveal :: HexContent -> HexContent
hexReveal HexContent { .. } =
  HexContent { hexEnemies = bagMap reveal hexEnemies
             , hexRuins   = fmap reveal hexRuins
             , ..
             }
  where reveal (_,e) = (Revealed,e)

hexAddEnemy :: Visibility -> Enemy -> HexContent -> HexContent
hexAddEnemy v e HexContent { .. } =
  HexContent { hexEnemies = bagAdd 1 (v,e) hexEnemies, .. }

hexRemoveEnemy :: Enemy -> HexContent -> HexContent
hexRemoveEnemy e HexContent { .. } =
  HexContent { hexEnemies = fromMaybe hexEnemies
                               (bagRemove 1 (Revealed,e) hexEnemies), .. }

hexSetRuins :: Visibility -> Ruins -> HexContent -> HexContent
hexSetRuins v r HexContent { .. } = HexContent { hexRuins = Just (v,r), .. }

hexRemoveRuins :: HexContent -> HexContent
hexRemoveRuins HexContent { .. } = HexContent { hexRuins = Nothing, .. }



