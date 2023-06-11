module Deed.Action.Concentration where

import Deed.Action

concentrationBasic :: DeedDef
concentrationBasic s =
  askInputs "[Concentration] Choose mana color:"
    [ defOpt s (AskMana m) ("Gain" <+> pp m <+> "mana.") (gainMana m)
    | m <- map BasicMana [ Blue, White, Red ]
    ]

