{-# LANGUAGE RecordWildCards #-}
module MageKnight.HexContent where

import           MageKnight.Common
import           MageKnight.Enemies
import           MageKnight.Ruins
import           MageKnight.Player
import           MageKnight.Bag
import           MageKnight.ResourceQ (ResourceQ)
import qualified MageKnight.ResourceQ as RQ

import           Data.Maybe (fromMaybe)
import           Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map

data HexContent = HexContent
  { hexShields  :: [ PlayerName ] -- in reversed order
  , hexEnemies  :: Bag (Visibility, Enemy)
  , hexRuins    :: Maybe (Visibility, Ruins)
  , hexPlayers  :: Set Player
  }

hexEmpty :: HexContent
hexEmpty = HexContent
  { hexShields = []
  , hexEnemies = bagEmpty
  , hexRuins   = Nothing
  , hexPlayers = Set.empty
  }

hexAddShield :: PlayerName -> HexContent -> HexContent
hexAddShield s HexContent { .. } =
  HexContent { hexShields = s : hexShields, .. }

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

hexAddPlayer :: Player -> HexContent -> HexContent
hexAddPlayer p HexContent { .. } =
  HexContent { hexPlayers = Set.insert p hexPlayers, .. }

hexRemovePlayer :: Player -> HexContent -> HexContent
hexRemovePlayer p HexContent { .. } =
  HexContent { hexPlayers = Set.delete p hexPlayers, .. }


--------------------------------------------------------------------------------

hexWithRuins :: Time -> ResourceQ Ruins -> (HexContent, ResourceQ Ruins)
hexWithRuins time q =
  case RQ.take q of
    Just (r,q1) -> let v = case time of
                             Day   -> Revealed
                             Night -> Hidden
                   in (hexSetRuins v r hexEmpty, q1)
    Nothing     -> (hexEmpty, q)




hexAddEnemyFromPool :: Visibility -> EnemyType -> (HexContent, EnemyPool) ->
                                                  (HexContent, EnemyPool)
hexAddEnemyFromPool v et (hex,pool) =
  fromMaybe (hex,pool) $ do q      <- Map.lookup et pool
                            (e,q1) <- RQ.take q
                            return (hexAddEnemy v e hex, Map.insert et q1 pool)

hexWithEnemy :: Visibility -> EnemyType -> EnemyPool -> (HexContent, EnemyPool)
hexWithEnemy v et p = hexAddEnemyFromPool v et (hexEmpty, p)

hexWithCity :: BasicMana -> Int -> EnemyPool -> (HexContent, EnemyPool)
hexWithCity color level pool
  | level < 1  = hexWithCity color 1  pool
  | level > 11 = hexWithCity color 11 pool
  | otherwise = foldr (hexAddEnemyFromPool Hidden) (hexEmpty,pool) $ (!! level)
  $ case color of
      White -> [ e 0 Guardian $ e 1 Citizen []
               , e 1 Guardian $ e 1 Citizen []
               , e 0 Guardian $ e 2 Citizen []
               , e 2 Guardian $ e 1 Citizen []
               , e 1 Guardian $ e 2 Citizen []
               , e 3 Guardian $ e 1 Citizen []
               , e 2 Guardian $ e 2 Citizen []
               , e 1 Guardian $ e 3 Citizen []
               , e 3 Guardian $ e 2 Citizen []
               , e 2 Guardian $ e 3 Citizen []
               , e 1 Guardian $ e 4 Citizen []
               ]
      Blue ->  [ e 1 Guardian $ e 1 Mage                  []
               , e 2 Mage                                 []
               , e 1 Mage     $ e 1 Citizen               []
               , e 1 Guardian $ e 1 Mage    $ e 1 Citizen []
               , e 2 Mage     $ e 1 Citizen               []
               , e 1 Mage     $ e 2 Citizen               []
               , e 1 Guardian $ e 2 Mage    $ e 1 Citizen []
               , e 2 Mage     $ e 2 Citizen               []
               , e 1 Mage     $ e 3 Citizen               []
               , e 1 Guardian $ e 2 Mage    $ e 2 Citizen []
               , e 2 Mage     $ e 3 Citizen               []
               ]
      Green -> [ e 1 Guardian   $ e 1 Underworld               []
               , e 2 Underworld                                []
               , e 2 Guardian   $ e 1 Underworld               []
               , e 1 Guardian   $ e 1 Underworld $ e 1 Citizen []
               , e 2 Underworld $ e 1 Citizen                  []
               , e 2 Guardian   $ e 1 Underworld $ e 1 Citizen []
               , e 1 Guardian   $ e 2 Underworld $ e 1 Citizen []
               , e 2 Underworld $ e 2 Citizen                  []
               , e 1 Guardian   $ e 3 Underworld $ e 1 Citizen []
               , e 1 Guardian   $ e 2 Underworld $ e 2 Citizen []
               , e 2 Underworld $ e 3 Citizen                  []
               ]
      Red ->   [ e 1 Citizen                             []
               , e 1 Underworld $ e 1 Mage               []
               , e 1 Underworld $ e 1 Citizen            []
               , e 1 Underworld $ e 2 Mage               []
               , e 1 Underworld $ e 1 Mage $ e 1 Citizen []
               , e 2 Underworld $ e 2 Mage               []
               , e 1 Underworld $ e 2 Mage $ e 1 Citizen []
               , e 1 Underworld $ e 1 Mage $ e 2 Citizen []
               , e 2 Underworld $ e 2 Mage $ e 1 Citizen []
               , e 2 Underworld $ e 1 Mage $ e 2 Citizen []
               , e 1 Underworld $ e 1 Mage $ e 3 Citizen []
               ]
  where e n t = (replicate n t ++)








