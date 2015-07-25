{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module MageKnight.Deed
  ( Deed (..)
  , DeedType(..)
  , DeedName
  , wound
  , actionDeed
  , advancedActionDeed
  , spellDeed
  , artifactDeed

  , deedRules
  ) where

import MageKnight.Common
import MageKnight.Rule
import MageKnight.Bag
import MageKnight.JSON

import           Data.Text ( Text )
import qualified Data.Text as Text



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


wound :: Deed
wound = Deed { deedName      = "Wound"
             , deedNamePower = Nothing
             , deedType      = Wound
             , deedBasic     = []
             , deedPower     = []
             }

actionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
actionDeed color deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = Action color
       , ..
       }

advancedActionDeed :: BasicMana -> DeedName -> [Rule] -> [Rule] -> Deed
advancedActionDeed color deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , ..
       }



spellDeed :: BasicMana -> DeedName -> DeedName -> [Rule] -> [Rule] -> Deed
spellDeed color deedName powerName deedBasic deedPower =
  Deed { deedNamePower = Just powerName
       , deedType      = Spell color
       , ..
       }

artifactDeed :: DeedName -> [Rule] -> [Rule] -> Deed
artifactDeed deedName deedBasic deedPower =
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , ..
       }




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
    [ "use for 1 movement" === [DeedInHand deedName] --> [Movement]
    , "use for 1 influece" === [DeedInHand deedName] --> [Influence]
    , "use for 1 attack"   === [DeedInHand deedName] --> [Attack Melee Physycal]
    , "use for 1 block"    === [DeedInHand deedName] --> [Block Physycal]
    ]

  powerRuleName = case deedNamePower of
                    Nothing -> Text.append "Advanced " deedName
                    Just nm -> nm

  basicRules =
    [ Rule { ruleName = deedName
           , ruleIn   = bagAdd 1 (DeedInHand deedName) ruleIn
           , ..
           } | Rule { .. } <- deedBasic ]

  actionPowerRules c =
    [ Rule { ruleName = powerRuleName
           , ruleIn   = bagAdd 1 (DeedInHand deedName)
                      $ bagAdd 1 (ManaToken (BasicMana c)) ruleIn
           , ..
           } | Rule { .. } <- deedPower
    ]

  spellBasicRules c =
    [ Rule { ruleIn   = bagAdd 1 (ManaToken (BasicMana c)) ruleIn, ..  }
        | Rule { .. } <- basicRules ]

  spellPowerRules c =
    [ onlyDuring Night Rule { ruleIn  = bagAdd 1 (ManaToken Black) ruleIn, .. }
        | Rule { .. } <- actionPowerRules c
    ]

  artifactPowerRules =
    [ Rule { ruleName = powerRuleName
           , ruleOut  = bagAdd 1 (DeedDestroyed deedName) ruleOut
           , ..
           } | Rule { .. } <- deedPower
    ]



