{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
module MageKnight.Rule where

import MageKnight.Common
import MageKnight.Bag

import           Data.Text (Text)
import qualified Data.Text as Text
import           Control.Monad (foldM)
import           Text.PrettyPrint


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
    [ ManaCrystal b ] --> [ ManaToken (BasicMana b), SpentManaCrystal b ]
  | b <- anyBasicMana
  ]




