module Deed.Action.Tranquility where

import Optics

import Game.State
import Game.Input
import Deed.Action

doTranqulity :: Int -> State -> Interact ()
doTranqulity n s = askInputsMaybe_ "Choose Tranquility action:"
                                                    (optHeal ++ optDraw)
  where
  opt i help act = defOpt s (AskText (toText i)) help act

  optHeal =
    [ opt "Heal" ("Gain" <+> pp n <+> "healing points.") (gainHeal n)
    | case view phase s of
        ActionPhase (CombatAction {}) -> False
        _ -> True
    ]

  optDraw =
    [ opt "Draw" ("Draw" <+> pp n <+> "cards.") (drawCards n) ]
