module Hero where

import Common.Enum

data Hero =
    Arythea
  | Tovak
  | Goldyx
  | Norowas
  | Wolfhawk
  | Krang
    deriving (Eq,Ord,Show,Read)

declareEnumText ''Hero




