module Terrain.Hex
  ( -- * Basics
    HexContent
  , hexEmpty

    -- * Ownership
  , hexAddShield
  , hexHasShield

    -- * Enemies
  , hexWithEnemy
  , hexAddEnemyFromPool
  , hexReveal
  , hexTakeEnemies
  , hexHasEnemies
  , hexActiveEnemies

    -- * Ruins
  , hexWithRuins
  , hexRemoveRuins
  , hexRuinsObjective

    -- * Cities
  , hexWithCity
  ) where

import           GHC.Generics
import           Data.Maybe ( fromMaybe )
import           Data.Map(Map)
import qualified Data.Map as Map

import Data.Aeson qualified as JS

import KOI.Bag
import KOI.ResourceQ

import Common
import Mana.Type
import Enemies
import Ruins






-- | The contents of a single hex cell.
data HexContent = HexContent
  { hexShield   :: Bool
  , hexEnemies  :: Bag (Visibility, Enemy)
  , hexRuins    :: Maybe (Visibility, Ruins)
  }

-- | An empty hex cell.
hexEmpty :: HexContent
hexEmpty = HexContent
  { hexShield  = False
  , hexEnemies = bagEmpty
  , hexRuins   = Nothing
  }

--------------------------------------------------------------------------------
hexAddShield :: HexContent -> HexContent
hexAddShield h = h { hexShield = True }

hexHasShield :: HexContent -> Bool
hexHasShield = hexShield

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
  HexContent { hexEnemies = bagChange 1 (v,e) hexEnemies, .. }

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



-- | Add some ruins to the cell.
hexSetRuins :: Visibility -> Ruins -> HexContent -> HexContent
hexSetRuins v r HexContent { .. } = HexContent { hexRuins = Just (v,r), .. }

-- | Remove ruins from the cell.
hexRemoveRuins :: HexContent -> HexContent
hexRemoveRuins HexContent { .. } = HexContent { hexRuins = Nothing, .. }

-- | What needs to be completed to get a reward for the ruins.
hexRuinsObjective :: HexContent -> [ Objective ]
hexRuinsObjective HexContent { .. } =
  case hexRuins of
    Nothing    -> []
    Just (_, Ruins { .. } ) -> bagToList ruinsIn

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
  | otherwise = foldr (hexAddEnemyFromPool Hidden) (hexEmpty,pool)
                                                              $ (!! (level-1))
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

data RuinsView = HiddenRuins | VisibleRuins Ruins
  deriving (Generic,JS.ToJSON)

data EnemyView = HiddenEnemy EnemyType | VisibleEnemy Enemy
  deriving (Eq,Ord,Generic,JS.ToJSON)

data HexContentView = HexContentView
  { hShield       :: Bool
  , hEnemies      :: [ (EnemyView, Int) ]
  , hRuins        :: Maybe RuinsView
  }
  deriving (Generic,JS.ToJSON)

mkView :: HexContent -> HexContentView
mkView hc = HexContentView
  { hShield  = hexShield hc
  , hEnemies = es
  , hRuins  = mkRuins <$> hexRuins hc
  }

  where
  es = Map.toList
      $ Map.fromListWith (+)
      [ (case vis of
           Revealed -> VisibleEnemy e
           Hidden   -> HiddenEnemy (enemyType e)
                , n)
               | ((vis,e),n) <- bagToNumList (hexEnemies hc) ]

  mkRuins (vis,r) =
    case vis of
      Revealed -> VisibleRuins r
      Hidden   -> HiddenRuins


instance JS.ToJSON HexContent where
  toJSON = JS.toJSON . mkView


