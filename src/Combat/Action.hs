module Combat.Action where

import KOI.Field

import Game.KOI
import Game.State
import Combat
import Utils


combatOptions :: State -> [ InputOption () ]
combatOptions s =
  case getField phase s of
    ActionPhase (CombatAction combat) ->
      case getField combatPhase combat of
        Attacking a -> undefined
        Blocking  b -> undefined
        AssigningDamage d -> undefined
    _ -> []



