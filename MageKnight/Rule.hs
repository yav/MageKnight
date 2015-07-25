{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
module MageKnight.Rule where

import MageKnight.Common
import MageKnight.Bag

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (foldM)
import           Text.PrettyPrint


data TerrainCostChange =
    DecreaseTo Int      -- ^ Decrease to the given value
  | DecreaseBy Int Int  -- ^ Decrease by amount, to minimum
    deriving (Eq,Ord,Show)


data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana

  | ManaSource Mana
  | ManaDie

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element

  | Healing
  | ReadyUnit Int   -- ^ Ready a unit of this level

  | ADeed DeedName

  | Blocking EnemyName

  | ChangeTerrainCost Terrain TerrainCostChange

  | DrawDeed

  -- End of turn
  | ReputationLoss
  | RegainUsedCrystals
  | ManaSourceFixed Mana    -- Not re-rolled
  | FameGainIfInteract        -- ^ Noble Manners
  | ReputationGainIfInteract  -- ^ Noble Manners
  | ThrowAway DeedName
  | ToDeedDeckBottom DeedName
  | ToDeedDeckTop DeedName
    deriving (Eq,Ord,Show)


-- Pretty Print

ppResource :: Resource -> Doc
ppResource resource =
  case resource of
    ManaToken m -> ppMana m <+> text "token"
    ManaCrystal m -> ppBasicMana m <+> text "crystal"
    RegainUsedCrystals -> text "regain unused crystals"
    ManaSource m -> ppMana m <+> text "source"
    ManaSourceFixed m -> ppMana m <+> text "source (fixed)"
    ManaDie -> text "mana die"

    Movement -> text "movement"
    Influence -> text "influence"
    Attack at el -> elDoc <+> tyDoc <+> text "attack"
      where elDoc = ppElement el
            tyDoc = case at of
                      Melee  -> empty
                      Ranged -> text "ranged"
                      Siege  -> text "siege"
    Block e  -> ppElement e
    Healing  -> text "healing"
    ADeed x  -> text (show x) <+> text "card"

    -- XXX
    ChangeTerrainCost t c -> text "change terrain cost"

    Blocking x -> text "blocking" <+> text (Text.unpack x)

    ReputationLoss -> text "reputation -1"
    DrawDeed -> text "draw a card"
    FameGainIfInteract -> text "fame +1 (if interacted)"
    ReputationGainIfInteract -> text "reputation + 1 (if interacted)"
    ThrowAway x -> text "throw away" <+> text (show x)
    ToDeedDeckBottom x -> text "place" <+> text (show x) <+>
                          text "at the bottom of the deed deck"

    ToDeedDeckTop x -> text "place" <+> text (show x) <+>
                       text "at the top of the deed deck"



ppResources :: Bag Resource -> Doc
ppResources = vcat . map ppEntry . bagToListGrouped
  where ppEntry (r,x) = int x <+> ppResource r








data Rule = Rule
  { ruleName  :: Text
  , ruleIn    :: Bag Resource
  , ruleOut   :: Bag Resource
  }

useRule :: Rule -> Bag Resource -> Maybe (Bag Resource)
useRule Rule { .. } rAvail =
  bagUnion ruleOut `fmap` foldM rm rAvail (bagToListGrouped ruleIn)
  where
  rm rs (r,q) = bagRemove q r rs

ppRule :: Rule -> Doc
ppRule Rule { .. } = vcat [ text (Text.unpack ruleName) <> text ":"
                          , nest 2 (ppResources ruleIn)
                          , text "-->"
                          , nest 2 (ppResources ruleOut)
                          ]



infix 1 ===
infix 2 -->

(-->) :: [Resource] -> [Resource] -> Rule
rIn --> rOut = Rule { ruleName = ""
                    , ruleIn   = bagFromList rIn
                    , ruleOut  = bagFromList rOut
                    }

(===) :: Text -> Rule -> Rule
name === rule = rule { ruleName = name }


rules :: [ Rule ]
rules =

  -- XXX: Only during the day
  [ Text.pack ("gold -> " ++ show (ppBasicMana b)) ===
    [ ManaToken Gold ] --> [ ManaToken (BasicMana b) ]
  | b <- anyBasicMana
  ]
  ++
  [ Text.pack ("take " ++ show (ppMana m) ++ " die") ===
    [ ManaDie, ManaSource m ] --> [ ManaToken m ]
  | m <- anyMana
  ]
  ++
  [ Text.pack ("use " ++ show (ppBasicMana b) ++ " crystal") ===
    [ ManaCrystal b ] --> [ ManaToken (BasicMana b) ]
  | b <- anyBasicMana
  ]




