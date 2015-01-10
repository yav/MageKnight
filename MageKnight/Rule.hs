{-# LANGUAGE Safe, OverloadedStrings, RecordWildCards #-}
module MageKnight.Rule where

import MageKnight.Common
import MageKnight.Bag

import Data.Text (Text)
import Control.Monad (foldM)


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

infix 1 ===
infix 2 -->

(-->) :: [Resource] -> [Resource] -> Rule
rIn --> rOut = Rule { ruleName = ""
                    , ruleIn   = bagFromList rIn
                    , ruleOut  = bagFromList rOut
                    }

(===) :: Text -> Rule -> Rule
name === rule = rule { ruleName = name }



