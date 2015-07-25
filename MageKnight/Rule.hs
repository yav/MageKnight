{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
module MageKnight.Rule where

import MageKnight.Common
import MageKnight.Bag

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (foldM)
import           Text.PrettyPrint



--------------------------------------------------------------------------------



data Rule = Rule
  { ruleName  :: Text
  , ruleIn    :: Bag Resource   -- ^ We consume these
  , ruleOut   :: Bag Resource   -- ^ We produce these
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


-- Rule DSL --------------------------------------------------------------------

infix 1 ===
infixr 2 &&&
infix 3 -->

-- | Define an anonymous rule.
(-->) :: [Resource] -> [Resource] -> Rule
rIn --> rOut = Rule { ruleName = ""
                    , ruleIn   = bagFromList rIn
                    , ruleOut  = bagFromList rOut
                    }

-- | Set the name for a rule.
(===) :: Text -> Rule -> Rule
name === rule = rule { ruleName = name }


-- | Join two rules together.  The resulting rule has the requirements
-- for both rules, and the results of both rules.  The name is computed
-- by concatenating the two rules.
(&&&) :: Rule -> Rule -> Rule
r1 &&& r2 = Rule { ruleIn   = bagUnion (ruleIn r1) (ruleIn r2)
                 , ruleOut  = bagUnion (ruleOut r1) (ruleOut r2)
                 , ruleName = Text.append (ruleName r1) (ruleName r2)
                 }

-- | Used to restrict when other rules work.
timeIs :: Time -> Rule
timeIs t = [TimeIs t] --> [TimeIs t]

-- | Usefd to add additional resource requirement.
requires :: [ Resource ] -> Rule
requires rs = rs --> []

produces :: [ Resource ] -> Rule
produces rs = [] --> rs


rules :: [ Rule ]
rules =

  [ Text.pack ("gold -> " ++ show (ppBasicMana b)) ===
    timeIs Day &&& [ ManaToken Gold ] --> [ ManaToken (BasicMana b) ]
  | b <- anyBasicMana
  ]
  ++
  [ Text.pack ("take " ++ show (ppMana m) ++ " die") ===
    [ ManaDie, ManaSource m ] --> [ ManaToken m ]
  | m <- anyMana
  ]
  ++
  [ Text.pack ("use " ++ show (ppBasicMana b) ++ " crystal") ===
    [ ManaCrystal b ] --> [ ManaToken (BasicMana b), UsedCrystal b ]
  | b <- anyBasicMana
  ]


--------------------------------------------------------------------------------

data TerrainCostChange =
    DecreaseTo Int      -- ^ Decrease to the given value
  | DecreaseBy Int Int  -- ^ Decrease by amount, to minimum
    deriving (Eq,Ord,Show)


data Resource =

    ManaToken Mana
  | ManaCrystal BasicMana
  | UsedCrystal BasicMana       -- ^ A used crystal of this type

  | ManaSource Mana             -- ^ Mana in the source
  | ManaDie                     -- ^ Access to the source

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element
  | Fame
  | Healing

  | Hand [DeedName]
  | DeedInHand DeedName         -- ^ A specific card
  | DeedDestroyed DeedName      -- ^ This is out of the game
  | DeedDiscarded DeedName      -- ^ This was discarded

  | TimeIs Time                 -- ^ Time of day

{-
  | ReadyUnit Int               -- ^ Ready a unit of this level



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
  -}
    deriving (Eq,Ord,Show)


-- Pretty Print

ppResource :: Resource -> Doc
ppResource resource =
  case resource of
    ManaToken m   -> ppMana m <+> text "token"
    ManaCrystal m -> ppBasicMana m <+> text "crystal"
    UsedCrystal m -> text "used" <+> ppBasicMana m <+> text "crystal"
    ManaSource m  -> ppMana m <+> text "in the source"
    ManaDie       -> text "use of source"

    Movement      -> text "movement"
    Influence     -> text "influence"
    Attack at el -> elDoc <+> tyDoc <+> text "attack"
      where elDoc = ppElement el
            tyDoc = case at of
                      Melee  -> empty
                      Ranged -> text "ranged"
                      Siege  -> text "siege"
    Block e  -> ppElement e <+> text "block"
    Healing  -> text "healing"
    Fame     -> text "fame"

    DeedInHand x    -> text (show x) <+> text "card"
    DeedDestroyed x -> text (show x) <+> text "was destroyed"
    DeedDiscarded x -> text (show x) <+> text "was discarded"

    TimeIs t        -> text "it is" <+> ppTime t

    {-
    ManaSourceFixed m -> ppMana m <+> text "source (fixed)"
    RegainUsedCrystals -> text "regain unused crystals"

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

    -}

ppResources :: Bag Resource -> Doc
ppResources = vcat . map ppEntry . bagToListGrouped
  where ppEntry (r,x) = int x <+> ppResource r





