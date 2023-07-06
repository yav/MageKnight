module Deed.Action
  ( module Deed.Action
  , module X
  ) where

import Data.Text(Text)
import Data.Set qualified as Set
import Data.Map qualified as Map
import Optics

import KOI.PP                       as X
import KOI.Basics                   as X

import Common                       as X
import Mana.Type                    as X
import Mana.Pool
import Game.KOI                     as X hiding (view)
import Game.State                   as X
import Game.Input                   as X
import Combat                       as X


data DeedAction = DeedAction
  { actBasic :: Interact ()
  , actPower :: Interact ()
  }

type DeedDef = State -> Interact ()

defOpt ::
  State -> Input -> Doc -> Interact a -> (WithPlayer Input, Text, Interact a)
defOpt s i t k = (playerId s :-> i, toText t, k)

defDeed :: DeedDef -> DeedDef -> DeedAction
defDeed a b = DeedAction { actBasic = mk a, actPower = mk b }
  where
  mk f = getState >>= f

deedNotImplemented :: Text -> DeedAction
deedNotImplemented txt = DeedAction
  { actBasic = notImplemented txt
  , actPower = notImplemented txt
  }

 -- XXX: say something, when we have logging
notImplemented :: Text -> Interact ()
notImplemented _ = pure ()

gainMove :: Int -> Interact ()
gainMove n =
  do updateThe_ movement (n +)

gainHeal :: Int -> Interact ()
gainHeal n =
  do updateThe_ heal (n +)

gainMana :: Mana -> Interact ()
gainMana m =
  do updateThe_ mana (addMana m)

drawCards :: Int -> Interact ()
drawCards = undefined -- XXX

-- | Select attack targets.  Does nothing if we already have a selection.
-- Automatically selects the enemy if there is only one available.
selectAttackTargets :: Interact ()
selectAttackTargets =
  do s <- getState
     case view phase s of
       ActionPhase (CombatAction combat)
         | Attacking attack <- view combatPhase combat
         , Set.null (view attackGroup attack) ->
           case [ eid | (eid,e) <- Map.toList (view combatEnemies combat)
                      , view enemyAlive e ] of
             [ eid ] -> doSet s combat [eid]
             -- XXX:
       _ -> pure ()

  where
  doSet s c g =
    update
      $ SetState
      $ set phase
          ( ActionPhase
          $ CombatAction
          $ updateAttackPhase (set attackGroup (Set.fromList g)) c
          )
        s



