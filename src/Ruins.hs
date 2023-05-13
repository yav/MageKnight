module Ruins
  ( Objective(..)
  , Reward(..)
  , Ruins(..)
  , ruins
  ) where

import Data.Text(Text)
import GHC.Generics
import Data.Aeson qualified as JS

import KOI.Bag
import KOI.Enum (declareEnumText)


import Mana.Type
import Enemies


data Objective = GiveMana BasicMana
               | Fight EnemyType
                deriving (Eq,Ord,Show)

data Reward   = RewardFame
              | RewardCrystal BasicMana
              | RewardArtifact
              | RewardAdvancedAction
              | RewardSpell
              | RewardUnit
                deriving (Eq,Ord,Show)

declareEnumText ''Objective
declareEnumText ''Reward

data Ruins = Ruins
  { ruinsName :: Text
  , ruinsIn   :: Bag Objective
  , ruinsOut  :: Bag Reward
  } deriving (Show, Generic, JS.ToJSON)

instance Eq Ruins where
  x == y = ruinsName x == ruinsName y

instance Ord Ruins where
  compare x y = compare (ruinsName x) (ruinsName y)

infix 1 ===
infix 2 -->

(-->) :: [Objective] -> [Reward] -> Ruins
rIn --> rOut = Ruins { ruinsName = ""
                     , ruinsIn   = bagFromList rIn
                     , ruinsOut  = bagFromList rOut
                     }

(===) :: Text -> Ruins -> Ruins
name === rs = rs { ruinsName = name }

donate :: [BasicMana] -> Int -> Ruins
donate xs n = map GiveMana xs --> replicate n RewardFame

altar :: BasicMana -> Ruins
altar y = donate (replicate 3 y) 7

fight :: [EnemyType] -> [Reward] -> Ruins
fight xs zs = map Fight xs --> zs

rewardCrystals :: [ Reward ]
rewardCrystals = [ RewardCrystal c | c <- anyBasicMana ]

ruins :: [Ruins]
ruins =
  [ "1"  === altar Green
  , "2"  === altar Blue
  , "3"  === altar White
  , "4"  === altar Red
  , "5"  === fight [Guardian   ,Mage      ] [ RewardUnit ]
  , "6"  === fight [Orc        ,Underworld] [ RewardArtifact ]
  , "7"  === fight [Orc        ,Orc       ] rewardCrystals
  , "8"  === fight [Underworld ,Mage      ] (RewardSpell : rewardCrystals)
  , "9"  === fight [Guardian   ,Citizen   ] [ RewardArtifact, RewardSpell ]
  , "10" === fight [Guardian   ,Underworld] [ RewardArtifact ]
  , "11" === fight [Underworld ,Draconum  ] [ RewardArtifact, RewardArtifact ]
  , "12" === fight [Orc        ,Draconum  ]
                                        [ RewardArtifact,RewardAdvancedAction]

    -- LL
  , "13" === donate anyBasicMana 10
  , "14" === fight (replicate 3 Orc) [ RewardUnit ]
  , "15" === fight (replicate 2 Mage) [ RewardSpell, RewardAdvancedAction ]
  ]




