module Common where

import Text.PrettyPrint


import KOI.Enum

data Usable     = Unused | Used
                  deriving (Show,Eq)

data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physical | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)

data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)

data Time       = Day | Night
                  deriving (Eq,Ord,Show)


-- | Information about what sort of damage are we assigning.
data DamageInfo = DamageInfo { damageElement    :: Element
                             , damagePoisons    :: Bool
                             , damageParalyzes  :: Bool
                             }

anyAttack :: [ AttackType ]
anyAttack = [ Melee, Ranged, Siege ]

ppElement :: Element -> Doc
ppElement el =
  case el of
    Physical  -> text "physycal"
    Fire      -> text "fire"
    Ice       -> text "ice"
    ColdFire  -> text "cold fire"

ppTime :: Time -> Doc
ppTime t =
  case t of
    Day   -> text "day"
    Night -> text "night"

--------------------------------------------------------------------------------

declareEnumText ''Usable
declareEnumText ''Element
declareEnumText ''AttackType
declareEnumText ''Time


