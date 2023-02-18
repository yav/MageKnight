module Common where

import Util.JSON

import           Data.Text ( Text )
import qualified Data.Text as Text
import           Text.PrettyPrint
import           Data.Char(toLower)


type PlayerId   = Int
type DeedName   = Text
type EnemyName  = Text


data Usable     = Unused | Used
                  deriving (Show,Eq)

data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physycal | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show)

data Terrain    = Plains | Hills | Forest | Wasteland | Desert | Swamp
                | Lake | Mountain
                | Ocean {- for tile A and B -}
                  deriving (Eq,Ord,Show)


data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)


data BasicMana  = Red | Green | Blue | White
                  deriving (Eq,Ord,Show)

data Mana       = BasicMana BasicMana | Gold | Black
                  deriving (Eq,Ord,Show)

data Time       = Day | Night
                  deriving (Eq,Ord,Show)


-- | Information about what sort of damage are we assigning.
data DamageInfo = DamageInfo { damageElement    :: Element
                             , damagePoisons    :: Bool
                             , damageParalyzes  :: Bool
                             }



anyBasicMana :: [ BasicMana ]
anyBasicMana = [ Red, Green, Blue, White ]

anyMana :: [ Mana ]
anyMana = Gold : Black : map BasicMana anyBasicMana

anyAttack :: [ AttackType ]
anyAttack = [ Melee, Ranged, Siege ]



ppElement :: Element -> Doc
ppElement el =
  case el of
    Physycal  -> text "physycal"
    Fire      -> text "fire"
    Ice       -> text "ice"
    ColdFire  -> text "cold fire"

ppBasicMana :: BasicMana -> Doc
ppBasicMana = text . map toLower . show

ppMana :: Mana -> Doc
ppMana m =
  case m of
    BasicMana b -> ppBasicMana b
    Gold        -> text "gold"
    Black       -> text "black"

ppTime :: Time -> Doc
ppTime t =
  case t of
    Day   -> text "day"
    Night -> text "night"

--------------------------------------------------------------------------------

instance ExportAsKey Time where
  toKeyJS t = case t of
                Day   -> "day"
                Night -> "night"

instance Export Time where
  toJS = jsKey

instance ExportAsKey BasicMana where
  toKeyJS m = case m of
                White -> "white"
                Red   -> "red"
                Blue  -> "blue"
                Green -> "green"

instance Export BasicMana where
  toJS = jsKey

instance ExportAsKey Mana where
  toKeyJS m = case m of
                Gold        -> "gold"
                Black       -> "black"
                BasicMana b -> toKeyJS b

instance Export Mana where
  toJS = jsKey


instance ExportAsKey Element where
  toKeyJS m = case m of
                Physycal    -> "physical"
                Fire        -> "fire"
                Ice         -> "ice"
                ColdFire    -> "cold_fire"

instance Export Element where
  toJS = jsKey

instance ExportAsKey AttackType where
  toKeyJS m = case m of
                Melee   -> "melee"
                Ranged  -> "ranged"
                Siege   -> "siege"

instance Export AttackType where
  toJS = jsKey

instance ExportAsKey Terrain where
  toKeyJS t =
    case t of
      Plains    -> "plains"
      Hills     -> "hills"
      Forest    -> "forest"
      Wasteland -> "wasteland"
      Desert    -> "desert"
      Swamp     -> "swamp"
      Lake      -> "lake"
      Mountain  -> "mountain"
      Ocean     -> "ocean"


