module Deed
  ( Deed (..)
  , DeedType(..)
  , deedColor
  , DeedName
  , wound
  , actionDeed
  , advancedActionDeed
  , spellDeed
  , artifactDeed

  , ActiveDeed(..)
  , ActiveWay(..)
  , SimpleDeed(..)

  ) where

import Common
import Util.JSON

import           Data.Text ( Text )

type DeedName = Text

-- | A deed that has been played to get its benefits
data ActiveDeed = ActiveDeed
  { baseDeed  :: Deed       -- ^ The card that was played
  , activeWay :: ActiveWay  -- ^ How it was played
  }

data ActiveWay  = ActiveBase          -- ^ Basic ability, or day spell.
                | ActivePower         -- ^ Powered up card, or night spell.
                | ActiveAs SimpleDeed -- ^ Play "sideways"

data SimpleDeed = Move1 | Attack1 | Block1 | Influence1


data Deed     = Deed { deedName      :: DeedName
                     , deedNamePower :: Maybe DeedName -- ^ For spells
                     , deedType      :: DeedType
                     }

deedColor :: Deed -> Maybe BasicMana
deedColor d =
  case deedType d of
    Action b         -> Just b
    AdvancedAction b -> Just b
    Spell b          -> Just b
    _                -> Nothing

data DeedType = Wound | Action BasicMana | AdvancedAction BasicMana
              | Spell BasicMana | Artifact

instance Eq Deed where
  x == y = deedName x == deedName y

instance Ord Deed where
  compare x y = compare (deedName x) (deedName y)

instance Export Deed where
  toJS Deed { .. } = object [ "name" .= deedName, "type" .= deedType ]

instance Export DeedType where
  toJS ty = toJS $ case ty of
                     Wound            -> "wound" :: Text
                     Action _         -> "action"
                     AdvancedAction _ -> "action"
                     Spell _          -> "spell"
                     Artifact         -> "artifact"


-- | Make a wound.
wound :: Deed
wound = Deed { deedName      = "Wound"
             , deedNamePower = Nothing
             , deedType      = Wound
             }

-- | Make a basic action.
actionDeed :: BasicMana -> DeedName -> Deed
actionDeed color deedName =
  Deed { deedNamePower = Nothing
       , deedType      = Action color
       , ..
       }

-- | Make an advanced action.
advancedActionDeed :: BasicMana -> DeedName -> Deed
advancedActionDeed color deedName =
  Deed { deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , ..
       }


-- | Make a spell.
spellDeed :: BasicMana -> DeedName -> DeedName -> Deed
spellDeed color deedName powerName =
  Deed { deedNamePower = Just powerName
       , deedType      = Spell color
       , ..
       }

-- | Make an artifact.
artifactDeed :: DeedName -> Deed
artifactDeed deedName =
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , ..
       }

