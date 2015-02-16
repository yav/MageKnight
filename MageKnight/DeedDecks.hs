{-# LANGUAGE Safe, OverloadedStrings #-}
module MageKnight.DeedDecks
  ( Deed
  , advancedActions
  , spells
  , Spell.interactiveSpell
  , artifacts

  , makeDeck
  , arytheaDeck
  , goldyxDeck
  , norowasDeck
  , tovakDeck

  , allDeeds
  ) where

import           MageKnight.Deed
import qualified MageKnight.Action         as BasicAction
import qualified MageKnight.AdvancedAction as AdvancedAction
import qualified MageKnight.Spell          as Spell
import qualified MageKnight.Artifact       as Artifact

import           Data.Maybe (mapMaybe)
import           Data.List (find)

basicActions :: [Deed]
basicActions = BasicAction.blueSpecial
             : BasicAction.greenSpecial
             : BasicAction.redSpecial
             : BasicAction.whiteSpecial
             : BasicAction.deeds

advancedActions :: [Deed]
advancedActions = AdvancedAction.deeds

spells :: [Deed]
spells = Spell.deeds

artifacts :: [Deed]
artifacts = Artifact.deeds

-- | All cards that may appear in the deed deck, except for wounds.
allDeeds :: [Deed]
allDeeds = basicActions ++ advancedActions ++ spells ++ artifacts



-- | Make a deck, feature the given cards.  If we don't know about a card,
-- we ignore it.
makeDeck :: [DeedName] -> [Deed]
makeDeck = mapMaybe (\x -> find ((x ==) . deedName) allDeeds)

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

