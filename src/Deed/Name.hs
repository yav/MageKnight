module Deed.Name where

import KOI.Enum
{- HLINT ignore "Use camelCase" -}

data DeedName =
    WoundName
  | ActionName ActionName
  | AdvancedActionName  AdvancedActionName
  | SpellName SpellName
  | ArtifactName ArtifactName
    deriving (Eq,Ord)

data ActionName =

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
    deriving (Eq,Ord)


data AdvancedActionName =

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
    deriving (Eq,Ord)


data SpellName =

  -- Green
    Cure
  | Disease
  | Meditation
  | Trance
  | Tremor
  | Earthquake
  | Underground_Travel
  | Underground_Attack
  | Restoration
  | Rebirth

  -- Red
  | Demolish
  | Disintegrate
  | Burning_Shield
  | Exploding_Shield
  | Fireball
  | Firestorm
  | Flame_Wall
  | Flame_Wave
  | Offering
  | Sacrifice

  -- Blue
  | Chill
  | Lethal_Chill
  | Mistw_Form
  | Veil_of_Mist
  | Snowstorm
  | Blizzard
  | Space_Bending
  | Time_Bending
  | Mana_Bolt
  | Mana_Thunderbolt

  -- White
  | Expose
  | Mass_Expose
  | Call_to_Arms
  | Call_to_Glory
  | Charm
  | Possess
  | Whirlwind
  | Tornado
  | Wings_of_Wind
  | Wings_of_Night
    deriving (Eq,Ord)


data ArtifactName =
  XXXArtifactName
  deriving (Eq,Ord)


declareEnumText ''ActionName
declareEnumText ''AdvancedActionName
declareEnumText ''SpellName
declareEnumText ''ArtifactName
declareEnumText ''DeedName


