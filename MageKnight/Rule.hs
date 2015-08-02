{-# LANGUAGE Safe, RecordWildCards, FlexibleInstances, OverloadedStrings #-}
module MageKnight.Rule
  ( Rule
  , ruleOutput
  , ruleInput
  , useRule
  , ppRule
  , (===)
  , (&&&)
  , (-->)
  , (***)
  , requires
  , produces
  , onlyWhen
  , rules

  , Resource(..)
  , TerrainCostChange(..)
  , ppResource
  , ppResources
  ) where

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

instance Eq Rule where
  x == y = ruleName x == ruleName y

instance Ord Rule where
  compare x y = compare (ruleName x) (ruleName y)

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

infix  1 ===
infixr 2 &&&
infix  3 -->
infix  4 ***

-- | Define an anonymous rule.
(-->) :: (Resources take, Resources make) => take -> make -> Rule
rIn --> rOut = Rule { ruleName = ""
                    , ruleIn   = bagFromList (resources rIn)
                    , ruleOut  = bagFromList (resources rOut)
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

class Resources r where
  resources :: r -> [Resource]

instance Resources Resource where
  resources r = [r]

instance Resources (Int,Resource) where
  resources (n,r) = replicate n r

(***) :: Int -> Resource -> (Int,Resource)
x *** y = (x,y)

instance Resources a => Resources [a] where
  resources rs = concatMap resources rs


-- | A rule that ensure that some resources are present.
-- It does not modify the state.
onlyWhen :: Resources x => x -> Rule
onlyWhen rs = rs --> rs

-- | A rule that only requires the given resource.
requires :: Resources take => take -> Rule
requires rs = rs --> ([] :: [Resource])

-- | A rule that only produces the given resource.
produces :: Resources make => make -> Rule
produces rs = ([] :: [Resource]) --> rs


ruleInput :: Rule -> [(Int,Resource)]
ruleInput Rule { .. } = [ (n,x) | (x,n) <- bagToListGrouped ruleIn ]

ruleOutput :: Rule -> [(Int,Resource)]
ruleOutput Rule { .. } = [ (n,x) | (x,n) <- bagToListGrouped ruleOut ]


rules :: [ Rule ]
rules =

  [ Text.pack ("gold -> " ++ show (ppBasicMana b)) ===
    onlyWhen (TimeIs Day) &&& ManaToken Gold --> ManaToken (BasicMana b)

  | b <- anyBasicMana
  ]
  ++
  [ Text.pack ("take " ++ show (ppMana m) ++ " die") ===
    [ ManaDie, ManaSource m ] --> ManaToken m
  | m <- anyMana
  ]
  ++
  [ Text.pack ("use " ++ show (ppBasicMana b) ++ " crystal") ===
    ManaCrystal b --> [ ManaToken (BasicMana b), UsedCrystal b ]
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
  | ManaSourceFixed Mana        -- ^ Not re-rolled
  | ManaDie                     -- ^ Access to the source

  | Movement
  | Influence
  | Attack AttackType Element
  | Block Element
  | Fame
  | Healing
  | Reputation
  | BadReputation               -- ^ Loose reputation at end of turn

  | DeedInHand    DeedName      -- ^ A specific card
  | DeedDestroyed DeedName      -- ^ This is out of the game
  | DeedDiscarded DeedName      -- ^ This was discarded

  | TimeIs Time                 -- ^ Time of day

  | DrawDeed

  | Blocking EnemyName          -- ^ We are blovking this enemy

  | RegainUsedCrystals
  | ChangeTerrainCost Terrain TerrainCostChange

  | IfInteracted [Resource]     -- ^ Generalize to end of turn?

  | IfNotResist Element [Resource]

  | DecreaseArmor               -- ^ Of the enemy that's being blocked.

  | ToDeedDeckBottom DeedName
  | ToDeedDeckTop DeedName

  | Ambush
  | PowerAmbush

  | ReadyUnit Int               -- ^ Ready a unit of this level

  | GainWound
    deriving (Eq,Ord)


-- Pretty Print

ppResource :: Resource -> Doc
ppResource resource =
  case resource of
    ManaToken m   -> ppMana m <+> text "token"
    ManaCrystal m -> ppBasicMana m <+> text "crystal"
    UsedCrystal m -> text "used" <+> ppBasicMana m <+> text "crystal"
    ManaSource m  -> ppMana m <+> text "in the source"
    ManaSourceFixed m -> ppMana m <+> text "source (fixed)"
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
    BadReputation -> text "bad reputation"
    Reputation    -> text "reputation"

    DeedInHand x    -> text (show x) <+> text "card"
    DeedDestroyed x -> text (show x) <+> text "was destroyed"
    DeedDiscarded x -> text (show x) <+> text "was discarded"

    TimeIs t        -> text "it is" <+> ppTime t

    DrawDeed -> text "draw a card"

    IfInteracted rs -> ppResources (bagFromList rs) <+> text ", if interacted"
    Blocking x -> text "blocking" <+> text (Text.unpack x)
    RegainUsedCrystals -> text "regain used crystals"
    ChangeTerrainCost t c -> text "change terrain cost"


ppResources :: Bag Resource -> Doc
ppResources = vcat . map ppEntry . bagToListGrouped
  where ppEntry (r,x) = int x <+> ppResource r





