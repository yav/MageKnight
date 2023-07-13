module Deed.Play (playDeed) where

import Game.KOI
import Hand
import Deed.Type
import Deed.Action
import Deed.Action.ManaPull
import Deed.Action.Tranquility
import Deed.Action.Rage


playDeed :: SelectedMode -> Deed -> Interact()
playDeed mode ca =
  case mode of
    SelectedSideways -> notImplemented "(sideways)"
    SelectedBasic    -> actBasic (deedAction ca)
    SelectedAdvanced -> actPower (deedAction ca)

class HasDeedAction a where
  deedAction :: a -> DeedAction

instance HasDeedAction Deed where
  deedAction deed =
    case deed of
      Wound             -> deedNotImplemented "Wound"
      BasicAction     a -> deedAction a
      AdvancedAction  a -> deedAction a
      Spell           a -> deedAction a
      Artifact        a -> deedAction a



instance HasDeedAction BasicAction where
  deedAction deed =
    case deed of

      March -> defDeed (const (gainMove 2)) (const (gainMove 4))
      Tranquility -> defDeed (doTranqulity 1) (doTranqulity 2)
      Concentration -> deedNotImplemented "basicDeed"
      Savage_Harvesting -> deedNotImplemented "basicDeed"
      Rejuvenate -> deedNotImplemented "basicDeed"
      Will_Focus -> deedNotImplemented "basicDeed"

      Swiftness -> deedNotImplemented "basicDeed"
      Promise -> deedNotImplemented "basicDeed"
      Mana_Draw -> deedNotImplemented "basicDeed"
      Swift_Reflexes -> deedNotImplemented "basicDeed"
      Noble_Manners -> deedNotImplemented "basicDeed"
      Mana_Pull -> defDeed basicManaPull powerManaPull

      Stamina -> defDeed (const (gainMove 2)) (const (gainMove 4))
      Crystallize -> deedNotImplemented "basicDeed"
      Determination -> deedNotImplemented "basicDeed"
      Tirelessness -> deedNotImplemented "basicDeed"
      Crystal_Joy -> deedNotImplemented "basicDeed"
      Cold_Toughness -> deedNotImplemented "basicDeed"

      Rage -> defDeed rageBasic ragePower
      Threaten -> deedNotImplemented "basicDeed"
      Improvisation -> deedNotImplemented "basicDeed"
      Battle_Versatility -> deedNotImplemented "basicDeed"
      Ruthless_Coercion -> deedNotImplemented "basicDeed"
      Instinct -> deedNotImplemented "basicDeed"

instance HasDeedAction AdvancedAction where
  deedAction deed =
    case deed of
      Crushing_Bolt -> deedNotImplemented "AdvancedAction"
      Refreshing_Walk -> deedNotImplemented "AdvancedAction"
      Path_Finding -> deedNotImplemented "AdvancedAction"
      Regeneration -> deedNotImplemented "AdvancedAction"
      In_Need -> deedNotImplemented "AdvancedAction"
      Ambush -> deedNotImplemented "AdvancedAction"
      Training -> deedNotImplemented "AdvancedAction"
      Stout_Resolve -> deedNotImplemented "AdvancedAction"
      Force_of_Nature -> deedNotImplemented "AdvancedAction"
      Mountain_Lore -> deedNotImplemented "AdvancedAction"

      Swift_Bolt -> deedNotImplemented "AdvancedAction"
      Agility -> deedNotImplemented "AdvancedAction"
      Song_of_Wind -> deedNotImplemented "AdvancedAction"
      Heroic_Tale -> deedNotImplemented "AdvancedAction"
      Diplomacy -> deedNotImplemented "AdvancedAction"
      Mana_Storm -> deedNotImplemented "AdvancedAction"
      Learning -> deedNotImplemented "AdvancedAction"
      Chivalry -> deedNotImplemented "AdvancedAction"
      Peaceful_Moment -> deedNotImplemented "AdvancedAction"
      Dodge_and_Weave -> deedNotImplemented "AdvancedAction"

      Ice_Bolt -> deedNotImplemented "AdvancedAction"
      Ice_Shield -> deedNotImplemented "AdvancedAction"
      Frost_Bridge -> deedNotImplemented "AdvancedAction"
      Pure_Magic -> deedNotImplemented "AdvancedAction"
      Steady_Tempo -> deedNotImplemented "AdvancedAction"
      Crystal_Mastery -> deedNotImplemented "AdvancedAction"
      Magic_Talent -> deedNotImplemented "AdvancedAction"
      Shield_Bash -> deedNotImplemented "AdvancedAction"
      Temporal_Portal -> deedNotImplemented "AdvancedAction"
      Spell_Forge -> deedNotImplemented "AdvancedAction"

      Fire_Bolt -> deedNotImplemented "AdvancedAction"
      Blood_Rage -> deedNotImplemented "AdvancedAction"
      Intimidate -> deedNotImplemented "AdvancedAction"
      Blood_Ritual -> deedNotImplemented "AdvancedAction"
      Into_the_Heat -> deedNotImplemented "AdvancedAction"
      Decompose -> deedNotImplemented "AdvancedAction"
      Maximal_Effect -> deedNotImplemented "AdvancedAction"
      Counterattack -> deedNotImplemented "AdvancedAction"
      Ritual_Attack -> deedNotImplemented "AdvancedAction"
      Blood_of_Ancients -> deedNotImplemented "AdvancedAction"

instance HasDeedAction Spell where
  deedAction deed =
    case deed of
      Cure -> deedNotImplemented "Spell"
      Meditation -> deedNotImplemented "Spell"
      Tremor -> deedNotImplemented "Spell"
      Underground_Travel -> deedNotImplemented "Spell"
      Restoration -> deedNotImplemented "Spell"

      Demolish -> deedNotImplemented "Spell"
      Burning_Shield -> deedNotImplemented "Spell"
      Fireball -> deedNotImplemented "Spell"
      Flame_Wall -> deedNotImplemented "Spell"
      Offering -> deedNotImplemented "Spell"

      Chill -> deedNotImplemented "Spell"
      Mist_Form -> deedNotImplemented "Spell"
      Snowstorm -> deedNotImplemented "Spell"
      Space_Bending -> deedNotImplemented "Spell"
      Mana_Bolt -> deedNotImplemented "Spell"

      Expose -> deedNotImplemented "Spell"
      Call_to_Arms -> deedNotImplemented "Spell"
      Charm -> deedNotImplemented "Spell"
      Whirlwind -> deedNotImplemented "Spell"
      Wings_of_Wind -> deedNotImplemented "Spell"

instance HasDeedAction Artifact where
  deedAction deed =
    case deed of
      XXXArtifact -> deedNotImplemented "Artifact"


