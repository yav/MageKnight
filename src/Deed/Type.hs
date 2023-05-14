module Deed.Type
  ( Deed (..)

  , wound
  , basicActions
  , advancedActions
  , spells
  , artifacts

  , deed
  , actionDeed
  , advancedActionDeed
  , spellDeed
  , artifactDeed

  , module Deed.Name
  ) where

import GHC.Generics
import Data.Text(Text)
import Data.Aeson(ToJSON)

import Mana.Type
import Deed.Name

data Deed = Deed
  { deedName      :: DeedName
  , deedNamePower :: Maybe Text     -- ^ For spells
  , deedColor     :: [BasicMana]
  } deriving (Generic,ToJSON)

instance Eq Deed where
  x == y = deedName x == deedName y

instance Ord Deed where
  compare x y = compare (deedName x) (deedName y)


deed :: DeedName -> Deed
deed name =
  case name of
    WoundName            -> wound
    ActionName a         -> actionDeed a
    AdvancedActionName a -> advancedActionDeed a
    SpellName a          -> spellDeed a
    ArtifactName a       -> artifactDeed a

-- | Make a wound.
wound :: Deed
wound =
  Deed { deedName      = WoundName
       , deedNamePower = Nothing
       , deedColor     = []
       }

-- | Make a basic action.
actionDeed :: ActionName -> Deed
actionDeed name =
  Deed { deedNamePower = Nothing
       , deedColor     = [color]
       , deedName      = ActionName name
       }
  where
  color =
    case name of
      March             ->  Green
      Tranquility       ->  Green
      Concentration     ->  Green
      Savage_Harvesting ->  Green
      Rejuvenate        ->  Green
      Will_Focus        ->  Green

      Swiftness ->  White
      Promise ->  White
      Mana_Draw ->  White
      Swift_Reflexes ->  White
      Noble_Manners ->  White
      Mana_Pull ->  White

      Stamina ->  Blue
      Crystallize ->  Blue
      Determination ->  Blue
      Tirelessness ->  Blue
      Crystal_Joy ->  Blue
      Cold_Toughness ->  Blue
 
      Rage ->  Red
      Threaten ->  Red
      Improvisation ->  Red
      Battle_Versatility ->  Red
      Ruthless_Coercion ->  Red
      Instinct ->  Red



-- | Make an advanced action.
advancedActionDeed :: AdvancedActionName -> Deed
advancedActionDeed name =
  Deed { deedNamePower = Nothing
       , deedColor     = [color]
       , deedName      = AdvancedActionName name
       }
  where
  color =
    case name of

      Crushing_Bolt -> Green
      Refreshing_Walk -> Green
      Path_Finding -> Green
      Regeneration -> Green
      In_Need -> Green
      Ambush -> Green
      Training -> Green
      Stout_Resolve -> Green
      Force_of_Nature -> Green
      Mountain_Lore -> Green

      Swift_Bolt -> White
      Agility -> White
      Song_of_Wind -> White
      Heroic_Tale -> White
      Diplomacy -> White
      Mana_Storm -> White
      Learning -> White
      Chivalry -> White
      Peaceful_Moment -> White
      Dodge_and_Weave -> White

      Ice_Bolt -> Blue
      Ice_Shield -> Blue
      Frost_Bridge -> Blue
      Pure_Magic -> Blue
      Steady_Tempo -> Blue
      Crystal_Mastery -> Blue
      Magic_Talent -> Blue
      Shield_Bash -> Blue
      Temporal_Portal -> Blue
      Spell_Forge -> Blue

      Fire_Bolt -> Red
      Blood_Rage -> Red
      Intimidate -> Red
      Blood_Ritual -> Red
      Into_the_Heat -> Red
      Decompose -> Red
      Maximal_Effect -> Red
      Counterattack -> Red
      Ritual_Attack -> Red
      Blood_of_Ancients -> Red


-- | Make a spell. XXX: Remove secondary spell ids
spellDeed :: SpellName -> Deed
spellDeed name =
  case name of
   Cure                -> mk Green Cure                "Disease"
   Meditation          -> mk Green Meditation          "Trance"
   Tremor              -> mk Green Tremor              "Earthquake"
   Underground_Travel  -> mk Green Underground_Travel  "Underground_Attack"
   Restoration         -> mk Green Restoration         "Rebirth"

   Demolish            -> mk White Demolish            "Disintegrate"
   Burning_Shield      -> mk White Burning_Shield      "Exploding_Shield"
   Fireball            -> mk White Fireball            "Firestorm"
   Flame_Wall          -> mk White Flame_Wall          "Flame_Wave"
   Offering            -> mk White Offering            "Sacrifice"

   Chill               -> mk Blue Chill                "Lethal_Chill"
   Mist_Form           -> mk Blue Mist_Form            "Veil_of_Mist"
   Snowstorm           -> mk Blue Snowstorm            "Blizzard"
   Space_Bending       -> mk Blue Space_Bending        "Time_Bending"
   Mana_Bolt           -> mk Blue Mana_Bolt            "Mana_Thunderbolt"

   Expose              -> mk Red Expose                "Mass_Expose"
   Call_to_Arms        -> mk Red Call_to_Arms          "Call_to_Glory"
   Charm               -> mk Red Charm                 "Possess"
   Whirlwind           -> mk Red Whirlwind             "Tornado"
   Wings_of_Wind       -> mk Red Wings_of_Wind         "Wings_of_Night"
  where
  mk color name1 name2 =
    Deed { deedNamePower = Just name2
         , deedColor     = [color]
         , deedName      = SpellName name1
         }


-- | Make an artifact.
artifactDeed :: ArtifactName -> Deed
artifactDeed name =
  Deed { deedNamePower = Nothing
       , deedColor     = []
       , deedName      = ArtifactName name
       }


basicActions :: [Deed]
basicActions = map actionDeed [ minBound .. maxBound ]

advancedActions :: [Deed]
advancedActions = map advancedActionDeed [ minBound .. maxBound ]

spells :: [Deed]
spells = map spellDeed [ minBound .. maxBound ]

artifacts :: [Deed]
artifacts = [] -- XXX



