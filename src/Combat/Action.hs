module Combat.Action where

import Optics

import Game.State
import Combat
import Utils


combatOptions :: State -> [ InputOption () ]
combatOptions s =
  case view phase s of
    ActionPhase (CombatAction combat) ->
      case view combatPhase combat of
        Attacking a -> attackingOptions s a
        Blocking b -> undefined
        AssigningDamage d -> undefined
    _ -> []

attackingOptions :: State -> OneAttack -> [InputOption ()]
attackingOptions s a = []



