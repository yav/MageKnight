module Common where

import GHC.Generics
import Data.Text ( Text )
import Control.Applicative((<|>))
import Control.Monad(guard)
import Text.PrettyPrint
import Data.Char(toLower)


import Data.Aeson
import Data.Aeson.Types

import Common.Utils


type DeedName   = Text
type EnemyName  = Text


data Usable     = Unused | Used
                  deriving (Show,Eq)

data Visibility = Revealed | Hidden
                  deriving (Eq,Ord,Show)

data Element    = Physycal | Fire | Ice | ColdFire
                  deriving (Eq,Ord,Show,Generic,ToJSON)


data AttackType = Melee | Ranged | Siege
                  deriving (Eq,Ord,Show)


data BasicMana  = Red | Green | Blue | White
                  deriving (Eq,Ord,Show,Read,Generic,ToJSON,FromJSON)

data Mana       = BasicMana BasicMana | Gold | Black
                  deriving (Eq,Ord,Show,Read,Generic)

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
instance ToJSONKey BasicMana where toJSONKey = jsDeriveKey

instance ToJSONKey Mana where
  toJSONKey = toJSONKeyText \m ->
    case m of
      BasicMana a -> showText a
      _           -> showText m

instance ToJSON Mana where
  toJSON m =
    case m of
      BasicMana a -> toJSON a
      _           -> toJSON (showText m)

instance FromJSON Mana where
  parseJSON v = (BasicMana <$> parseJSON v)
             <|> check Gold
             <|> check Black
    where
    check y =
      withText "special mana" (\t -> guard (t == showText y) >> pure y) v


