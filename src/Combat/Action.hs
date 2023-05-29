module Combat.Action where

import KOI.Field

import Game.KOI
import Game.State
import Combat
import Utils


combatOptions :: State -> [ InputOption () ]
combatOptions s =
  case getField phase s of
    ActionPhase (Combat combatPhase) ->
      case combatPhase of
        Attacking a -> undefined
        Blocking  b -> undefined
        AssigningDamage d -> undefined
    _ -> []

