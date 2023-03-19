module DeedDecks
  ( Deed
  , advancedActions
  , spells
  , actions
  , artifacts

  , makeCustomDeck
  , makeDeckFor
  , arytheaDeck
  , goldyxDeck
  , norowasDeck
  , tovakDeck
  , wolfhawkDeck
  , krangDeck

  , allDeeds
  , findDeed
  ) where

import Data.Maybe (mapMaybe)
import Data.List (find, (\\))

import Common
import Deed
import Player (PlayerName)



findDeed :: DeedName -> Maybe Deed
findDeed name = find ((name ==) . deedName) allDeeds

basicActions :: [Deed]
basicActions =
  map (actionDeed Green)
    [ "March", "Tranquility", "Concentration"
    , "Savage Harvesting", "Rejuvenate", "Will Focus"
    ] ++
  map (actionDeed White)
    [ "Swiftness", "Promise", "Mana Draw"
    , "Swift Reflexes", "Noble Manners", "Mana Pull"
    ] ++
  map (actionDeed Blue)
    [ "Stamina", "Crystallize", "Determination"
    , "Tirelessness", "Crystal Joy", "Cold Toughness"
    ] ++
  map (actionDeed Red)
    [ "Rage", "Threaten", "Improvisation"
    , "Battle Versatility", "Ruthless Coercion", "Instinct"
    ]

advancedActions :: [Deed]
advancedActions =
  map (advancedActionDeed Green)
    [ "Crushing Bolt"
    , "Refreshing Walk"
    , "Path Finding"
    , "Regeneration"
    , "In Need"
    , "Ambush"
    , "Training"
    , "Stout Resolve"
    , "Force of Nature"
    , "Mountain Lore"
    ] ++
  map (advancedActionDeed White)
    [ "Swift Bolt"
    , "Agility"
    , "Song of Wind"
    , "Heroic Tale"
    , "Diplomacy"
    , "Mana Storm"
    , "Learning"
    , "Chivalry"
    , "Peaceful Moment"
    , "Dodge and Weave"
    ] ++
  map (advancedActionDeed Blue)
    [ "Ice Bolt"
    , "Ice Shield"
    , "Frost Bridge"
    , "Pure Magic"
    , "Steady Tempo"
    , "Crystal Mastery"
    , "Magic Talent"
    , "Shield Bash"
    , "Temporal Portal"
    , "Spell Forge"
    ] ++
  map (advancedActionDeed Red)
    [ "Fire Bolt"
    , "Blood Rage"
    , "Intimidate"
    , "Blood Ritual"
    , "Into the Heat"
    , "Decompose"
    , "Maximal Effect"
    , "Counterattack"
    , "Ritual Attack"
    , "Blood of Ancients"
    ]


actions :: [Deed]
actions = basicActions ++ advancedActions

spells :: [Deed]
spells =
  map (spellDeed Green)
    [ "Cure"                +++ "Disease"
    , "Meditation"          +++ "Trance"
    , "Tremor"              +++ "Earthquake"
    , "Underground Travel"  +++ "Underground Attack"
    , "Restoration"         +++ "Rebirth"
    ] ++

  map (spellDeed Red)
    [ "Demolish"        +++ "Disintegrate"
    , "Burning Shield"  +++ "Exploding Shield"
    , "Fireball"        +++ "Firestorm"
    , "Flame Wall"      +++ "Flame Wave"
    , "Offering"        +++ "Sacrifice"
    ] ++

  map (spellDeed Blue)
    [ "Chill"           +++ "Lethal Chill"
    , "Mistw Form"      +++ "Veil of Mist"
    , "Snowstorm"       +++ "Blizzard"
    , "Space Bending"   +++ "Time Bending"
    , "Mana Bolt"       +++ "Mana Thunderbolt"
    ] ++

  map (spellDeed White)
    [ "Expose"          +++ "Mass Expose"
    , "Call to Arms"    +++ "Call to Glory"
    , "Charm"           +++ "Possess"
    , "Whirlwind"       +++ "Tornado"
    , "Wings of Wind"   +++ "Wings of Night"
    ]

  where (+++) = (,)

artifacts :: [Deed]
artifacts = [] -- XXX

-- | All cards that may appear in the deed deck
allDeeds :: [Deed]
allDeeds = wound : basicActions ++ advancedActions ++ spells ++ artifacts



-- | Make a deck with the given cards.  If we don't know about a card,
-- we ignore it.
makeCustomDeck :: [DeedName] -> [Deed]
makeCustomDeck = mapMaybe (\x -> find ((x ==) . deedName) allDeeds)

makeDeckFor :: PlayerName -> Maybe [Deed]
makeDeckFor name =
  do deedNames <- case name of
                    "Arythea"   -> Just arytheaDeck
                    "Tovak"     -> Just tovakDeck
                    "goldyx"    -> Just goldyxDeck
                    "Norowas"   -> Just norowasDeck
                    "Wolfhawk"  -> Just wolfhawkDeck
                    "Krang"     -> Just krangDeck
                    _           -> Nothing
     return (makeCustomDeck deedNames)

-- | A started deck with no special cards.
basicDeck :: [(DeedName,DeedName)] -> [ DeedName ]
basicDeck changes = add ++ (basic \\ rm)
  where
  (rm,add) = unzip changes
  basic    = [ "Stamina", "Stamina", "Determination", "Crystallize"
             , "March", "March", "Concentration", "Tranquility"
             , "Rage", "Rage", "Improvisation", "Threaten"
             , "Swiftness", "Swiftness", "Promise", "Mana Draw"
             ]

-- | Starter deck for Arythea.
arytheaDeck :: [ DeedName ]
arytheaDeck =
  basicDeck
    [ ("Rage",      "Battle Versatility")
    , ("Mana Draw", "Mana Pull")
    ]

-- | Starter deck for Goldyx.
goldyxDeck :: [ DeedName ]
goldyxDeck =
  basicDeck
    [ ("Concentration", "Will Focus")
    , ("Crystallize",    "Crystal Joy")
    ]

-- | Starter deck for Tovak.
tovakDeck :: [ DeedName ]
tovakDeck =
  basicDeck
    [ ("Determination", "Cold Toughness")
    , ("Improvisation", "Instinct")
    ]

-- | Starter deck for Norowas.
norowasDeck :: [ DeedName ]
norowasDeck =
  basicDeck
    [ ("Promise", "Noble manners")
    , ("Tranquility", "Rejuvenate")
    ]

-- | Starter deck for Wolfhawk.
wolfhawkDeck :: [ DeedName ]
wolfhawkDeck =
  basicDeck
    [ ("Swiftness", "Swift Reflexes")
    , ("Stamina", "Tirelessness")
    ]

krangDeck :: [ DeedName ]
krangDeck =
  basicDeck
    [ ("March", "Savage Harvesting")
    , ("Threaten", "Ruthless Coercion")
    ]
