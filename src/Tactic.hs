{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Tactic where

import Common

import Data.Text

data Tactic = Tactic
  { tacticNumber    :: Int
  , tacticName      :: Text
  , tacticActivates :: TacticActivates
  , tacticUsed      :: Usable
  -- XXX: What does it do
  }

data TacticActivates = TacticImmediate | TacticOther

dayTactics :: [Tactic]
dayTactics =
  [ Tactic { tacticNumber     = 1
           , tacticName       = "Early Bird"
           , tacticActivates  = TacticImmediate
           , .. }

  , Tactic { tacticNumber     = 2
           , tacticName       = "Rethink"
           , tacticActivates  = TacticImmediate
           , .. }

  , Tactic { tacticNumber     = 3
           , tacticName       = "Mana Steal"
           , tacticActivates  = TacticOther
           , .. }

  , Tactic { tacticNumber     = 4
           , tacticName       = "Planning"
           , tacticActivates  = TacticOther
           , .. }

  , Tactic { tacticNumber     = 5
           , tacticName       = "Great Start"
           , tacticActivates  = TacticImmediate
           , .. }

  , Tactic { tacticNumber     = 6
           , tacticName       = "The Right Moment"
           , tacticActivates = TacticOther
           , .. }
  ]
  where
  tacticUsed = Unused

nightTactics :: [Tactic]
nightTactics =
  [ Tactic { tacticNumber     = 1
           , tacticName       = "From The Dusk"
           , tacticActivates  = TacticImmediate
           , .. }

  , Tactic { tacticNumber     = 2
           , tacticName       = "Long Night"
           , tacticActivates  = TacticOther
           , .. }

  , Tactic { tacticNumber     = 3
           , tacticName       = "Mana Search"
           , tacticActivates  = TacticOther
           , .. }

  , Tactic { tacticNumber     = 4
           , tacticName       = "Midnight Meditation"
           , tacticActivates  = TacticOther
           , .. }

  , Tactic { tacticNumber     = 5
           , tacticName       = "Preparation"
           , tacticActivates  = TacticImmediate
           , .. }

  , Tactic { tacticNumber     = 6
           , tacticName       = "Sparing Power"
           , tacticActivates  = TacticOther
           , .. }
  ]
  where
  tacticUsed = Unused
