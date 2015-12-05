{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
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

  , deedRules
  ) where

import Common
import Rule
import Util.JSON

import           Data.Text ( Text )
import qualified Data.Text as Text


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
                     , deedBasic     :: [ Rule ]
                     , deedPower     :: [ Rule ]
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
             , deedBasic     = []
             , deedPower     = []
             }

-- | Make a basic action.
actionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
actionDeed color deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = Action color
       , ..
       }

-- | Make an advanced action.
advancedActionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
advancedActionDeed color deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , ..
       }


-- | Make a spell.
spellDeed :: BasicMana -> DeedName -> DeedName -> [Rule] -> [Rule] -> Deed
spellDeed color deedName powerName deedBasic deedPower =
  Deed { deedNamePower = Just powerName
       , deedType      = Spell color
       , ..
       }

-- | Make an artifact.
artifactDeed :: DeedName -> [Rule] -> [Rule] -> Deed
artifactDeed deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , ..
       }

--------------------------------------------------------------------------------



-- | All possible ways to play a deed.  When we define an individual deed,
-- we don't encode rules that are common to all deeds of the given type.
-- It is here that we add these common patterns.
deedRules :: Deed -> [Rule]
deedRules Deed { .. } =
  case deedType of
    Wound            -> []
    Action c         -> sidewaysRules ++ basicRules ++ actionPowerRules c
    AdvancedAction c -> sidewaysRules ++ basicRules ++ actionPowerRules c
    Spell c          -> sidewaysRules ++ spellBasicRules c ++ spellPowerRules c
    Artifact         -> sidewaysRules ++ basicRules ++ artifactPowerRules

  where
  sidewaysRules =
    [ "use for 1 movement" === DeedInHand deedName --> Movement
    , "use for 1 influece" === DeedInHand deedName --> Influence
    , "use for 1 attack"   === DeedInHand deedName --> Attack Melee Physycal
    , "use for 1 block"    === DeedInHand deedName --> Block Physycal
    ]

  powerRuleName = case deedNamePower of
                    Nothing -> Text.append "Advanced " deedName
                    Just nm -> nm

  basicRules =
    [ deedName === requires (DeedInHand deedName) &&& r | r <- deedBasic ]

  actionPowerRules c =
    [ powerRuleName ===
        requires [ DeedInHand deedName, ManaToken (BasicMana c) ] &&& r
                                                            | r <- deedPower ]

  spellBasicRules c =
    [ requires [ ManaToken (BasicMana c) ] &&& r | r <- basicRules ]

  spellPowerRules c =
    [ onlyWhen (TimeIs Night) &&& requires (ManaToken Black) &&& r
                                                    | r <- actionPowerRules c ]

  artifactPowerRules =
    [ powerRuleName ===
        produces (DeedDestroyed deedName) &&& r | r <- deedPower ]


