module Hero where

import KOI.Enum

data Hero =
    Arythea
  | Tovak
  | Goldyx
  | Norowas
  | Wolfhawk
  | Krang
    deriving (Eq,Ord,Show,Read)

declareEnumText ''Hero




