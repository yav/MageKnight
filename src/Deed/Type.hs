module Deed.Type
  ( Deed (..)
  , DeedType(..)
  , deedColor
  , wound
  , basicActions
  , advancedActions
  , spells
  , artifacts

  , actionDeed
  , advancedActionDeed
  , spellDeed
  , artifactDeed
  , makeCustomDeck
  , basicDeck

  , module Deed.Name
  ) where

import GHC.Generics
import Data.Aeson(ToJSON)
import Data.List(find,(\\))
import Data.Maybe(mapMaybe)

import Mana.Type
import Deed.Name

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
  Deed { deedName      = WoundName
       , deedNamePower = Nothing
       , deedType      = Wound
       }

-- | Make a basic action.
actionDeed :: BasicMana -> ActionName -> Deed
actionDeed color name =
  Deed { deedNamePower = Nothing
       , deedType      = Action color
       , deedName      = ActionName name
       }

-- | Make an advanced action.
advancedActionDeed :: BasicMana -> AdvancedActionName -> Deed
advancedActionDeed color name =
  Deed { deedNamePower = Nothing
       , deedType      = AdvancedAction color
       , deedName      = AdvancedActionName name
       }


-- | Make a spell.
spellDeed :: BasicMana -> (SpellName,SpellName) -> Deed
spellDeed color (name1,name2) =
  Deed { deedNamePower = Just (SpellName name2)
       , deedType      = Spell color
       , deedName      = SpellName name1
       }

-- | Make an artifact.
artifactDeed :: ArtifactName -> Deed
artifactDeed name =
  Deed { deedNamePower = Nothing
       , deedType      = Artifact
       , deedName      = ArtifactName name
       }


findDeed :: DeedName -> Maybe Deed
findDeed name = find ((name ==) . deedName) allDeeds

basicActions :: [Deed]
basicActions =
  map (actionDeed Green)
    [ March, Tranquility, Concentration
    , Savage_Harvesting, Rejuvenate, Will_Focus
    ] ++
  map (actionDeed White)
    [ Swiftness, Promise, Mana_Draw
    , Swift_Reflexes, Noble_Manners, Mana_Pull
    ] ++
  map (actionDeed Blue)
    [ Stamina, Crystallize, Determination
    , Tirelessness, Crystal_Joy, Cold_Toughness
    ] ++
  map (actionDeed Red)
    [ Rage, Threaten, Improvisation
    , Battle_Versatility, Ruthless_Coercion, Instinct
    ]

advancedActions :: [Deed]
advancedActions =
  map (advancedActionDeed Green)
    [ Crushing_Bolt
    , Refreshing_Walk
    , Path_Finding
    , Regeneration
    , In_Need
    , Ambush
    , Training
    , Stout_Resolve
    , Force_of_Nature
    , Mountain_Lore
    ] ++
  map (advancedActionDeed White)
    [ Swift_Bolt
    , Agility
    , Song_of_Wind
    , Heroic_Tale
    , Diplomacy
    , Mana_Storm
    , Learning
    , Chivalry
    , Peaceful_Moment
    , Dodge_and_Weave
    ] ++
  map (advancedActionDeed Blue)
    [ Ice_Bolt
    , Ice_Shield
    , Frost_Bridge
    , Pure_Magic
    , Steady_Tempo
    , Crystal_Mastery
    , Magic_Talent
    , Shield_Bash
    , Temporal_Portal
    , Spell_Forge
    ] ++
  map (advancedActionDeed Red)
    [ Fire_Bolt
    , Blood_Rage
    , Intimidate
    , Blood_Ritual
    , Into_the_Heat
    , Decompose
    , Maximal_Effect
    , Counterattack
    , Ritual_Attack
    , Blood_of_Ancients
    ]


actions :: [Deed]
actions = basicActions ++ advancedActions

spells :: [Deed]
spells =
  map (spellDeed Green)
    [ Cure                +++ Disease
    , Meditation          +++ Trance
    , Tremor              +++ Earthquake
    , Underground_Travel  +++ Underground_Attack
    , Restoration         +++ Rebirth
    ] ++

  map (spellDeed Red)
    [ Demolish        +++ Disintegrate
    , Burning_Shield  +++ Exploding_Shield
    , Fireball        +++ Firestorm
    , Flame_Wall      +++ Flame_Wave
    , Offering        +++ Sacrifice
    ] ++

  map (spellDeed Blue)
    [ Chill           +++ Lethal_Chill
    , Mistw_Form      +++ Veil_of_Mist
    , Snowstorm       +++ Blizzard
    , Space_Bending   +++ Time_Bending
    , Mana_Bolt       +++ Mana_Thunderbolt
    ] ++

  map (spellDeed White)
    [ Expose          +++ Mass_Expose
    , Call_to_Arms    +++ Call_to_Glory
    , Charm           +++ Possess
    , Whirlwind       +++ Tornado
    , Wings_of_Wind   +++ Wings_of_Night
    ]

  where (+++) = (,)

artifacts :: [Deed]
artifacts = [] -- XXX

-- | All cards that may appear in the deed deck
allDeeds :: [Deed]
allDeeds = wound : basicActions ++ advancedActions ++ spells ++ artifacts



-- | Make a deck with the given cards.  If we don't know about a card,
-- we ignore it.
makeCustomDeck :: [ActionName] -> [Deed]
makeCustomDeck = mapMaybe (\x -> find ((ActionName x ==) . deedName) allDeeds)

-- | A started deck with no special cards.
basicDeck :: [(ActionName,ActionName)] -> [ ActionName ]
basicDeck changes = add ++ (basic \\ rm)
  where
  (rm,add) = unzip changes
  basic    = [ Stamina, Stamina, Determination, Crystallize
             , March, March, Concentration, Tranquility
             , Rage, Rage, Improvisation, Threaten
             , Swiftness, Swiftness, Promise, Mana_Draw
             ]


