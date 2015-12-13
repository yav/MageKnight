{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module HexContent
  ( -- * Basics
    HexContent
  , hexEmpty

    -- * Enemies
  , hexWithEnemy
  , hexAddEnemyFromPool
  , hexReveal
  , hexTakeEnemies
  , hexHasEnemies
  , hexActiveEnemies

    -- * Players
  , hexAddPlayer
  , hexRemovePlayer
  , hexHasPlayers

    -- * Shields
  , hexAddShield
  , hexRemoveShield
  , hexHasShield
  , hexOwners

    -- * Ruins
  , hexWithRuins
  , hexRemoveRuins
  , hexRuinsObjective

    -- * Cities
  , hexWithCity
  ) where

import Common
import Enemies
import Ruins
import Player

import Util.ResourceQ
import Util.JSON
import Util.Bag

import           Data.Maybe ( fromMaybe )
import           Data.List ( delete, sortBy, groupBy, nub )
import           Data.Function ( on )
import           Data.Set ( Set )
import qualified Data.Set as Set
import qualified Data.Map as Map
import           Data.Char ( toLower, isAlphaNum )
import qualified Data.Text as Text

-- | The contents of a single hex cell.
data HexContent = HexContent
  { hexShields  :: [ PlayerName ]
    -- ^ In reversed order.  The order is used to break ties when
    -- computing city leaders.

  , hexEnemies  :: Bag (Visibility, Enemy)
  , hexRuins    :: Maybe (Visibility, Ruins)
  , hexPlayers  :: Set Player
  }

-- | An empty hex cell.
hexEmpty :: HexContent
hexEmpty = HexContent
  { hexShields = []
  , hexEnemies = bagEmpty
  , hexRuins   = Nothing
  , hexPlayers = Set.empty
  }

-- | Add a shield for the given player.
hexAddShield :: PlayerName -> HexContent -> HexContent
hexAddShield s HexContent { .. } =
  HexContent { hexShields = s : hexShields, .. }

-- | Remove a shield for the given player.
hexRemoveShield :: PlayerName -> HexContent -> HexContent
hexRemoveShield s HexContent { .. } =
  HexContent { hexShields = delete s hexShields, .. }

-- | Does the player have a shield on this hex.
hexHasShield :: PlayerName -> HexContent -> Bool
hexHasShield s h = s `elem` hexShields h

{- | Find players that have shields on this hex.
If there are still monsters present, then we return no owners.
If there are multiple owners, the first player in the list
we be the one with most shields, or in the case of a draw
whoever got a shielf first (i.e., the city leader of a city). -}
hexOwners :: HexContent -> [PlayerName]
hexOwners HexContent { .. }
  | bagIsEmpty hexEnemies =
    case hexShields of
      [s] -> [s]    -- the common case

      -- For cities, and also Maze/Labyrint from expansion, there may be
      -- multiple shields. We arrange these so that the "city" leader
      -- is first in the list.
      _ -> concat
         $ map (reorder . map fst)  -- 
         $ groupBy ((==) `on` snd)  -- group by same number of shields
         $ sortBy moreFirst         -- sort players, most shields first
         $ bagToListGrouped         -- count shields per player
         $ bagFromList hexShields

  | otherwise = []

  where
  moreFirst (_,m) (_,n) = compare n m
  reorder ps            = filter (`elem` ps) (nub (reverse hexShields))

--------------------------------------------------------------------------------

-- | Reveal hidden enemies on this cell.
hexReveal :: HexContent -> HexContent
hexReveal HexContent { .. } =
  HexContent { hexEnemies = bagMap reveal hexEnemies
             , hexRuins   = fmap reveal hexRuins
             , ..
             }
  where reveal (_,e) = (Revealed,e)

-- | Add an enemy to a hex-cell.
hexAddEnemy :: Visibility -> Enemy -> HexContent -> HexContent
hexAddEnemy v e HexContent { .. } =
  HexContent { hexEnemies = bagAdd 1 (v,e) hexEnemies, .. }

-- | Remove all enemies from a hex cell.
hexTakeEnemies :: HexContent -> ([Enemy], HexContent)
hexTakeEnemies HexContent { .. } =
  ( map snd (bagToList hexEnemies), HexContent { hexEnemies = bagEmpty, .. })

-- | Are there any enemies on the hex?
hexHasEnemies :: HexContent -> Bool
hexHasEnemies HexContent { .. } = not (bagIsEmpty hexEnemies)

-- | The visible enemies on a hex.
hexActiveEnemies :: HexContent -> [Enemy]
hexActiveEnemies HexContent { .. } =
  [ e | (Revealed,e) <- bagToList hexEnemies ]

--------------------------------------------------------------------------------



-- | Add som eruins to the cell.
hexSetRuins :: Visibility -> Ruins -> HexContent -> HexContent
hexSetRuins v r HexContent { .. } = HexContent { hexRuins = Just (v,r), .. }

-- | Remove ruins from the cell.
hexRemoveRuins :: HexContent -> HexContent
hexRemoveRuins HexContent { .. } = HexContent { hexRuins = Nothing, .. }

-- | What needs to be completed to get a reward for the ruins.
hexRuinsObjective :: HexContent -> [ Objectve ]
hexRuinsObjective HexContent { .. } =
  case hexRuins of
    Nothing    -> []
    Just (_, Ruins { .. } ) -> bagToList ruinsIn

--------------------------------------------------------------------------------

-- | Add a player figure to the cell.
hexAddPlayer :: Player -> HexContent -> HexContent
hexAddPlayer p HexContent { .. } =
  HexContent { hexPlayers = Set.insert p hexPlayers, .. }

-- | Remove a player figure.
hexRemovePlayer :: Player -> HexContent -> HexContent
hexRemovePlayer p HexContent { .. } =
  HexContent { hexPlayers = Set.delete p hexPlayers, .. }

-- | Are there any players on this hex.
hexHasPlayers :: HexContent -> Bool
hexHasPlayers HexContent { .. } = not (Set.null hexPlayers)

--------------------------------------------------------------------------------

-- | Make a new hex, with some ruins on it.
hexWithRuins :: Time -> ResourceQ Ruins -> (HexContent, ResourceQ Ruins)
hexWithRuins time q =
  case rqTake q of
    Just (r,q1) -> let v = case time of
                             Day   -> Revealed
                             Night -> Hidden
                   in (hexSetRuins v r hexEmpty, q1)
    Nothing     -> (hexEmpty, q)


-- | Make a new hex with some enemy of the given type on it.
hexWithEnemy :: Visibility -> EnemyType -> EnemyPool -> (HexContent, EnemyPool)
hexWithEnemy v et p = hexAddEnemyFromPool v et (hexEmpty, p)

-- | Add an enemy to the given hex.
hexAddEnemyFromPool :: Visibility -> EnemyType -> (HexContent, EnemyPool) ->
                                                  (HexContent, EnemyPool)
hexAddEnemyFromPool v et (hex,pool) =
  fromMaybe (hex,pool) $ do q      <- Map.lookup et pool
                            (e,q1) <- rqTake q
                            return (hexAddEnemy v e hex, Map.insert et q1 pool)

-- | Make a new hex with a city on it.
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


--------------------------------------------------------------------------------

instance Export HexContent where
  toJS HexContent { .. } =
    object [ "shields" .= map toImage (reverse hexShields)
           , "enemies" .= fmap enemy      (bagToList hexEnemies)
           , "players" .= fmap (toImage . playerName) (Set.toList hexPlayers)
           , "ruins"   .= fmap jsruins     hexRuins
           ]
      where
      jsEnemy ty name = object [ "type" .= toJS ty
                               , "name" .= toImage name
                               ]

      enemy (v,e) = jsEnemy (enemyType e)
                  $ case v of
                      Hidden   -> "back"
                      Revealed -> enemyName e

      jsruins (v,r) = case v of
                        Hidden   -> "back"
                        Revealed -> toImage (ruinsName r)

      toImage = Text.map cvt

      cvt c
        | isAlphaNum c  = toLower c
        | otherwise     = '_'





