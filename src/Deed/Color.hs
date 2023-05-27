module Deed.Color where

import Mana.Type
import Deed.Type

class DeedColor a where
  deedColor :: a -> [BasicMana]

instance DeedColor Deed where
  deedColor name =
    case name of
      Wound -> []
      BasicAction a -> deedColor a
      AdvancedAction a -> deedColor a
      Spell a -> deedColor a
      Artifact {} -> []

instance DeedColor BasicAction where
  deedColor name =
    case name of
      March               -> [ Green ]
      Tranquility         -> [ Green ]
      Concentration       -> [ Green ]
      Savage_Harvesting   -> [ Green ]
      Rejuvenate          -> [ Green ]
      Will_Focus          -> [ Green ]
 
      Swiftness           ->  [ White ]
      Promise             ->  [ White ]
      Mana_Draw           ->  [ White ]
      Swift_Reflexes      ->  [ White ]
      Noble_Manners       ->  [ White ]
      Mana_Pull           ->  [ White ]

      Stamina             ->  [ Blue ]
      Crystallize         ->  [ Blue ]
      Determination       ->  [ Blue ]
      Tirelessness        ->  [ Blue ]
      Crystal_Joy         ->  [ Blue ]
      Cold_Toughness      ->  [ Blue ]

      Rage                ->  [ Red ]
      Threaten            ->  [ Red ]
      Improvisation       ->  [ Red ]
      Battle_Versatility  ->  [ Red ]
      Ruthless_Coercion   ->  [ Red ]
      Instinct            ->  [ Red ]

instance DeedColor AdvancedAction where
  deedColor name =
    case name of

      Crushing_Bolt      ->  [ Green ]
      Refreshing_Walk    ->  [ Green ]
      Path_Finding       ->  [ Green ]
      Regeneration       ->  [ Green ]
      In_Need            ->  [ Green ]
      Ambush             ->  [ Green ]
      Training           ->  [ Green ]
      Stout_Resolve      ->  [ Green ]
      Force_of_Nature    ->  [ Green ]
      Mountain_Lore      ->  [ Green ]

      Swift_Bolt         ->  [ White ]
      Agility            ->  [ White ]
      Song_of_Wind       ->  [ White ]
      Heroic_Tale        ->  [ White ]
      Diplomacy          ->  [ White ]
      Mana_Storm         ->  [ White ]
      Learning           ->  [ White ]
      Chivalry           ->  [ White ]
      Peaceful_Moment    ->  [ White ]
      Dodge_and_Weave    ->  [ White ]

      Ice_Bolt           ->  [ Blue ]
      Ice_Shield         ->  [ Blue ]
      Frost_Bridge       ->  [ Blue ]
      Pure_Magic         ->  [ Blue ]
      Steady_Tempo       ->  [ Blue ]
      Crystal_Mastery    ->  [ Blue ]
      Magic_Talent       ->  [ Blue ]
      Shield_Bash        ->  [ Blue ]
      Temporal_Portal    ->  [ Blue ]
      Spell_Forge        ->  [ Blue ]

      Fire_Bolt          ->  [ Red ]
      Blood_Rage         ->  [ Red ]
      Intimidate         ->  [ Red ]
      Blood_Ritual       ->  [ Red ]
      Into_the_Heat      ->  [ Red ]
      Decompose          ->  [ Red ]
      Maximal_Effect     ->  [ Red ]
      Counterattack      ->  [ Red ]
      Ritual_Attack      ->  [ Red ]
      Blood_of_Ancients  ->  [ Red ]

instance DeedColor Spell where
  deedColor name =
    case name of
     Cure                -> [ Green ]
     Meditation          -> [ Green ]
     Tremor              -> [ Green ]
     Underground_Travel  -> [ Green ]
     Restoration         -> [ Green ]

     Demolish            -> [ White ]
     Burning_Shield      -> [ White ]
     Fireball            -> [ White ]
     Flame_Wall          -> [ White ]
     Offering            -> [ White ]

     Chill               -> [ Blue ]
     Mist_Form           -> [ Blue ]
     Snowstorm           -> [ Blue ]
     Space_Bending       -> [ Blue ]
     Mana_Bolt           -> [ Blue ]

     Expose              -> [ Red ]
     Call_to_Arms        -> [ Red ]
     Charm               -> [ Red ]
     Whirlwind           -> [ Red ]
     Wings_of_Wind       -> [ Red ]
