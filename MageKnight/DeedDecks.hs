{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.DeedDecks
  ( Deed
  , advancedActions
  , spells
  , actions
  , Spell.interactiveSpell
  , artifacts

  , makeCustomDeck
  , makeDeckFor
  , arytheaDeck
  , goldyxDeck
  , norowasDeck
  , tovakDeck

  , allDeeds
  , findDeed
  ) where

import           MageKnight.Deed
import qualified MageKnight.Action         as BasicAction
import qualified MageKnight.AdvancedAction as AdvancedAction
import qualified MageKnight.Spell          as Spell
import qualified MageKnight.Artifact       as Artifact
import           MageKnight.Player (PlayerName)

import           Data.Maybe (mapMaybe)
import           Data.List (find)


findDeed :: DeedName -> Maybe Deed
findDeed name = find ((name ==) . deedName) allDeeds

basicActions :: [Deed]
basicActions = BasicAction.blueSpecial
             : BasicAction.greenSpecial
             : BasicAction.redSpecial
             : BasicAction.whiteSpecial
             : BasicAction.deeds

advancedActions :: [Deed]
advancedActions = AdvancedAction.deeds

actions :: [Deed]
actions = basicActions ++ advancedActions

spells :: [Deed]
spells = Spell.deeds

artifacts :: [Deed]
artifacts = Artifact.deeds

-- | All cards that may appear in the deed deck
allDeeds :: [Deed]
allDeeds = wound : basicActions ++ advancedActions ++ spells ++ artifacts



-- | Make a deck, feature the given cards.  If we don't know about a card,
-- we ignore it.
makeCustomDeck :: [DeedName] -> [Deed]
makeCustomDeck = mapMaybe (\x -> find ((x ==) . deedName) allDeeds)

makeDeckFor :: PlayerName -> Maybe [Deed]
makeDeckFor name =
  do deedNames <- case name of
                    "Arythea" -> Just arytheaDeck
                    "Tovak"   -> Just tovakDeck
                    "Goldyx"  -> Just goldyxDeck
                    "Norowas" -> Just norowasDeck
                    _         -> Nothing
     return (makeCustomDeck deedNames)

-- | A started deck with no special cards.
basicDeck :: [ DeedName ]
basicDeck = [ "Stamina", "Stamina", "Determination", "Crystalize"
            , "March", "March", "Concentration", "Tranquility"
            , "Rage", "Rage", "Improvisation", "Threaten"
            , "Swiftness", "Swiftness", "Promise", "Mana Draw"
            ]

-- | Starter deck for Arythea.
arytheaDeck :: [ DeedName ]
arytheaDeck = "Battle Versatility" : filter (/= "Improvisation") basicDeck

-- | Starter deck for Goldyx.
goldyxDeck :: [ DeedName ]
goldyxDeck = "Will Focus" : filter (/= "Concentration") basicDeck

-- | Starter deck for Tovak.
tovakDeck :: [ DeedName ]
tovakDeck = "Cold Toughness" : filter (/= "Determination") basicDeck

-- | Starter deck for Norowas.
norowasDeck :: [ DeedName ]
norowasDeck = "Noble manners" : filter (/= "Promise") basicDeck


