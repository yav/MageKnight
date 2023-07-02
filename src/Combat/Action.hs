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
        Attacking a -> attackingOptions s a
        Blocking  b -> undefined
        AssigningDamage d -> undefined
    _ -> []

attackingOptions :: State -> OneAttack -> [InputOption ()]
attackingOptions s a = []



