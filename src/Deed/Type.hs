module Deed.Type where

import Data.List((\\))
import KOI.Enum
{- HLINT ignore "Use camelCase" -}

allDeeds :: (Enum a, Bounded a) => (a -> Deed) -> [Deed]
allDeeds f = map f [ minBound .. maxBound ]

-- | A started deck with no special cards.
basicDeck :: [(BasicAction,BasicAction)] -> [ Deed ]
basicDeck changes = map BasicAction (add ++ (basic \\ rm))
  where
  (rm,add) = unzip changes
  basic    = [ Stamina, Stamina, Determination, Crystallize
             , March, March, Concentration, Tranquility
             , Rage, Rage, Improvisation, Threaten
             , Swiftness, Swiftness, Promise, Mana_Draw
             ]

--------------------------------------------------------------------------------

data Deed =
    Wound
  | BasicAction BasicAction
  | AdvancedAction AdvancedAction
  | Spell Spell
  | Artifact Artifact
    deriving (Eq,Ord,Read,Show)

data BasicAction =

   -- Green
     March
   | Tranquility
   | Concentration
   | Savage_Harvesting
   | Rejuvenate
   | Will_Focus

   -- White
   | Swiftness
   | Promise
   | Mana_Draw
   | Swift_Reflexes
   | Noble_Manners
   | Mana_Pull

   -- Blue
   | Stamina
   | Crystallize
   | Determination
   | Tirelessness
   | Crystal_Joy
   | Cold_Toughness

   -- Red
   | Rage
   | Threaten
   | Improvisation
   | Battle_Versatility
   | Ruthless_Coercion
   | Instinct
    deriving (Eq,Ord,Show,Read,Enum,Bounded)


data AdvancedAction =

  -- Green
    Crushing_Bolt
  | Refreshing_Walk
  | Path_Finding
  | Regeneration
  | In_Need
  | Ambush
  | Training
  | Stout_Resolve
  | Force_of_Nature
  | Mountain_Lore

  -- White
  | Swift_Bolt
  | Agility
  | Song_of_Wind
  | Heroic_Tale
  | Diplomacy
  | Mana_Storm
  | Learning
  | Chivalry
  | Peaceful_Moment
  | Dodge_and_Weave

  -- Blue
  | Ice_Bolt
  | Ice_Shield
  | Frost_Bridge
  | Pure_Magic
  | Steady_Tempo
  | Crystal_Mastery
  | Magic_Talent
  | Shield_Bash
  | Temporal_Portal
  | Spell_Forge

  -- Red
  | Fire_Bolt
  | Blood_Rage
  | Intimidate
  | Blood_Ritual
  | Into_the_Heat
  | Decompose
  | Maximal_Effect
  | Counterattack
  | Ritual_Attack
  | Blood_of_Ancients
    deriving (Eq,Ord,Show,Read,Enum,Bounded)


data Spell =

  -- Green
    Cure
  | Meditation
  | Tremor
  | Underground_Travel
  | Restoration

  -- Red
  | Demolish
  | Burning_Shield
  | Fireball
  | Flame_Wall
  | Offering

  -- Blue
  | Chill
  | Mist_Form
  | Snowstorm
  | Space_Bending
  | Mana_Bolt

  -- White
  | Expose
  | Call_to_Arms
  | Charm
  | Whirlwind
  | Wings_of_Wind
    deriving (Eq,Ord,Show,Read,Enum,Bounded)


data Artifact =
  XXXArtifact
  deriving (Eq,Ord,Show,Read,Enum,Bounded)


declareEnumText ''BasicAction
declareEnumText ''AdvancedAction
declareEnumText ''Spell
declareEnumText ''Artifact
declareEnumText ''Deed


