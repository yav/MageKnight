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

  ) where

import GHC.Generics
import Data.Aeson(ToJSON)

import Common

import           Data.Text ( Text )

type DeedName = Text

data Deed = Deed
  { deedName      :: DeedName
  , deedNamePower :: Maybe DeedName -- ^ For spells
  , deedType      :: DeedType
  } deriving (Generic,ToJSON)

deedColor :: Deed -> [BasicMana]
deedColor d =
  case deedType d of
    Action b         -> [b]
    AdvancedAction b -> [b]
    Spell b          -> [b]
    _                -> []

data DeedType =
    Wound
  | Action BasicMana
  | AdvancedAction BasicMana
  | Spell BasicMana
  | Artifact
    deriving (Generic, ToJSON)

instance Eq Deed where
  x == y = deedName x == deedName y

instance Ord Deed where
  compare x y = compare (deedName x) (deedName y)

-- | Make a wound.
wound :: Deed
wound =
  Deed { deedName      = "Wound"
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
spellDeed :: BasicMana -> (DeedName,DeedName) -> Deed
spellDeed color (deedName,powerName) =
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


