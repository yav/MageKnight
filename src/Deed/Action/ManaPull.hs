module Deed.Action.ManaPull where

import Optics

import Deed.Action
import Mana.Pool
import Mana.Source

basicManaPull :: DeedDef
basicManaPull s =
  askInputsMaybe_ "Mana Pull: Choose additional die to use."
    [ defOpt s (Source m) ("Use an additional" <+> pp m <+> "die.")
      do mv <- case m of
                 Black ->
                   inpMana <$>
                   choose pid "Mana Pull: Choose mana for black die."
                     [ ( AskMana tm
                       , toText ("Use black die for" <+> pp tm <+> "mana."))
                     | tm <- anyMana ]
                 _ -> pure m
         update
           $ SetState
           $ set source (takeMana m sourceVal)
           $ set mana   (addSourceMana mv manaVal)
             s

    | m <- avail
    ]
  where
  pid       = playerId s
  sourceVal = view source s
  manaVal   = view mana s
  avail     = availableMana sourceVal


powerManaPull :: DeedDef
powerManaPull = count 1
  where
  count :: Int -> State -> Interact ()
  count dieNum s
    | dieNum > 2 = pure ()
    | otherwise =
      askInputsMaybe_ (toText ("Mana Pull: Choose die" <+> pp dieNum <.> "/2."))
        [ defOpt s (Source m)
          ("Set and use" <+> pp m <+> "die.")
          do tgt <- inpMana <$>
                    choose pid (toText ("Mana Pull: New color for"
                                              <+> pp m <+> "die."))
                      [ (AskMana t,toText ("Set to" <+> pp t))
                      | t <- anyMana, t /= Gold ]
             update
               $ SetState
               $ set source (takeAndConvertMana m tgt sourceVal)
               $ set mana   (addMana tgt manaVal) s
             count (dieNum + 1) =<< getState
        | m <- avail
        ]
      where
      pid       = playerId s
      sourceVal = view source s
      manaVal   = view mana s
      avail     = availableMana sourceVal


