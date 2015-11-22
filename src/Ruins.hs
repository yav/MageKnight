{-# LANGUAGE OverloadedStrings, Safe #-}
module Ruins
  ( Objectve(..)
  , Reward(..)
  , Ruins(..)
  , ruins
  ) where

import Common
import Enemies

import Util.Bag

import Data.Text(Text)

data Objectve = GiveMana BasicMana
              | Fight EnemyType
                deriving (Eq,Ord,Show)

data Reward   = RewardFame
              | RewardCrystal BasicMana
              | RewardArtifact
              | RewardAdvancedAction
              | RewardSpell
              | RewardUnit
                deriving (Eq,Ord,Show)

data Ruins = Ruins
  { ruinsName :: Text
  , ruinsIn   :: Bag Objectve
  , ruinsOut  :: Bag Reward
  } deriving Show

instance Eq Ruins where
  x == y = ruinsName x == ruinsName y

instance Ord Ruins where
  compare x y = compare (ruinsName x) (ruinsName y)

infix 1 ===
infix 2 -->

(-->) :: [Objectve] -> [Reward] -> Ruins
rIn --> rOut = Ruins { ruinsName = ""
                     , ruinsIn   = bagFromList rIn
                     , ruinsOut  = bagFromList rOut
                     }

(===) :: Text -> Ruins -> Ruins
name === rs = rs { ruinsName = name }

altar :: BasicMana -> Ruins
altar y = replicate 3 (GiveMana y) --> replicate 7 RewardFame

fight :: EnemyType -> EnemyType -> [Reward] -> Ruins
fight x y zs = [ Fight x, Fight y ] --> zs

rewardCrystals :: [ Reward ]
rewardCrystals = [ RewardCrystal c | c <- anyBasicMana ]

ruins :: [Ruins]
ruins =
  [ "1"  === altar Green
  , "2"  === altar Blue
  , "3"  === altar White
  , "4"  === altar Red
  , "5"  === fight Guardian   Mage        [ RewardUnit ]
  , "6"  === fight Orc        Underworld  [ RewardArtifact ]
  , "7"  === fight Orc        Orc         rewardCrystals
  , "8"  === fight Underworld Mage        (RewardSpell : rewardCrystals)
  , "9"  === fight Guardian   Citizen     [ RewardArtifact, RewardSpell ]
  , "10" === fight Guardian   Underworld  [ RewardArtifact ]
  , "11" === fight Underworld Draconum    [ RewardArtifact, RewardArtifact ]
  , "12" === fight Orc        Draconum    [ RewardArtifact,RewardAdvancedAction]
  ]




