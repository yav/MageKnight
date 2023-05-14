module Hero where

import KOI.Enum
import Deed.Name
import Deed.Type

data Hero =
    Arythea
  | Tovak
  | Goldyx
  | Norowas
  | Wolfhawk
  | Krang
    deriving (Eq,Ord,Show,Read)

declareEnumText ''Hero


makeDeckFor :: Hero -> [Deed]
makeDeckFor name =
  map deed
    case name of

      Arythea   ->
        basicDeck
          [ (Rage,      Battle_Versatility)
          , (Mana_Draw, Mana_Pull)
          ]

      Tovak ->
        basicDeck
          [ (Determination, Cold_Toughness)
          , (Improvisation, Instinct)
          ]

      Goldyx ->
        basicDeck
          [ (Concentration, Will_Focus)
          , (Crystallize,   Crystal_Joy)
          ]

      Norowas ->
        basicDeck
          [ (Promise,     Noble_Manners)
          , (Tranquility, Rejuvenate)
          ]

      Wolfhawk ->
        basicDeck
          [ (Swiftness, Swift_Reflexes)
          , (Stamina,   Tirelessness)
          ]

      Krang ->
        basicDeck
          [ (March,    Savage_Harvesting)
          , (Threaten, Ruthless_Coercion)
          ]


