module Deed.Action.Tranquility where

import Game.State
import Game.Input
import Deed.Type
import Deed.Action

doTranqulity :: Int -> State -> Interact ()
doTranqulity n s = askInputsMaybe_ "Tranquility" (optHeal ++ optDraw)
  where
  opt i help act =
    defOpt s (AskDeed (BasicAction Tranquility) i) help act


  optHeal =
    [ opt "Heal" ("Gain" <+> pp n <+> "healing points.") (gainHeal n)
    | case getField phase s of
        ActionPhase (CombatAction {}) -> False
        _ -> True
    ]

  optDraw =
    [ opt "Draw" ("Draw" <+> pp n <+> "cards.") (drawCards n) ]

