{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module MageKnight.Cards where

import MageKnight.Common
import MageKnight.Enemies
import MageKnight.Rule
import MageKnight.Bag
import MageKnight.JSON

import qualified Data.Text as Text
import qualified Data.Set  as Set



data Card = Card { cardName  :: CardName
                 , cardColor :: BasicMana
                 , cardBasic :: [ Rule ]
                 , cardPower :: [ Rule ]
                 }

instance Eq Card where
  x == y = cardName x == cardName y

instance Ord Card where
  compare x y = compare (cardName x) (cardName y)

instance Export Card where
  toJS Card { .. } = toJS cardName




cardRules :: Card -> [Rule]
cardRules Card { .. } =
  [ "use for 1 movement" === [ACard cardName] --> [Movement]
  , "use for 1 influece" === [ACard cardName] --> [Influence]
  , "use for 1 attack"   === [ACard cardName] --> [Attack Melee Physycal]
  , "use for 1 block"    === [ACard cardName] --> [Block Physycal]
  ] ++
  [ Rule { ruleName = Text.append cardName " (basic)"
         , ruleIn   = bagAdd 1 (ACard cardName) ruleIn
         , ..
         } | Rule { .. } <- cardBasic
  ] ++
  [ Rule { ruleName = Text.append cardName " (power)"
         , ruleIn   = bagAdd 1 (ACard cardName)
                    $ bagAdd 1 (ManaToken (BasicMana cardColor)) ruleIn
         , ..
         } | Rule { .. } <- cardPower
  ]


basicDeck :: [ CardName ]
basicDeck = [ "Stamina", "Stamina", "Determination", "Crystalize"
            , "March", "March", "Concentration", "Tranquility"
            , "Rage", "Rage", "Improvisation", "Threaten"
            , "Swiftness", "Swiftness", "Promise", "Mana Draw"
            ]

arytheaDeck :: [ CardName ]
arytheaDeck = "Battle Versatility" : filter (/= "Improvisation") basicDeck

goldyxDeck :: [ CardName ]
goldyxDeck = "Will Focus" : filter (/= "Concentration") basicDeck

tovakDeck :: [ CardName ]
tovakDeck = "Cold Toughness" : filter (/= "Determination") basicDeck

norowasDeck :: [ CardName ]
norowasDeck = "Noble manners" : filter (/= "Promise") basicDeck

coldToughnessBlockBonus :: Enemy -> Int
coldToughnessBlockBonus Enemy  { .. } = fromAttack + fromAbilities
  where
  fromAttack    = case enemyAttack of
                    AttcaksWith e _ | e /= Physycal -> 1
                    _                               -> 0

  fromAbilities = Set.size enemyAbilities

cards :: [Card]
cards =


  [ Card
      { cardName  = "Stamina"
      , cardColor = Blue
      , cardBasic = [ [] --> replicate 2 Movement ]
      , cardPower = [ [] --> replicate 4 Movement ]
      }

  , Card
      { cardName  = "Determination"
      , cardColor = Blue
      , cardBasic = [ [] --> replicate 2 (Attack Melee Physycal)
                    , [] --> replicate 2 (Block Physycal)
                    ]
      , cardPower = [ [] --> replicate 5 (Block Physycal) ]
      }

  , Card
      { cardName  = "Crystalize"
      , cardColor = Blue
      , cardBasic = [ [ ManaToken (BasicMana b) ] --> [ ManaCrystal b ]
                    | b <- anyBasicMana
                    ]
      , cardPower = [ [] --> [ ManaCrystal b ]
                    | b <- anyBasicMana
                    ]
      }


  , Card
      { cardName  = "Cold Toughness"
      , cardColor = Blue
      , cardBasic = [ [] --> replicate 2 (Attack Melee Ice)
                    , [] --> replicate 3 (Block Ice)
                    ]
      , cardPower = [ [] --> replicate (5 + coldToughnessBlockBonus e)
                                       (Block Ice)
                    | e <- allEnemies
                    ]
      }



  , let name = "Concentration"
    in Card
    { cardName = name
    , cardColor = Green
    , cardBasic = [ [] --> [ ManaToken (BasicMana b) ]
                  | b <- [ Blue, White, Red ]
                  ]
    , cardPower = [ r | c <- cards, r <- concentrate 2 c ]
    }

  , Card
    { cardName = "March"
    , cardColor = Green
    , cardBasic = [ [] --> replicate 2 Movement ]
    , cardPower = [ [] --> replicate 4 Movement ]
    }

  , Card
      { cardName = "Tranquility"
      , cardColor = Green
      , cardBasic = [ [] --> [ Healing ], [] --> [ DrawCard ] ]
      , cardPower = [ [] --> replicate 2 Healing
                    , [] --> replicate 2 DrawCard
                    ]
      }

  , let name = "Will Focus"
    in Card
    { cardName  = name
    , cardColor = Green
    , cardBasic = ([] --> [ ManaCrystal Green ])
                : [ [] --> [ ManaToken (BasicMana b) ]
                  | b <- [ Blue, White, Red ]
                  ]
    , cardPower = [ r | c <- cards, r <- concentrate 3 c ]
    }



  , Card
    { cardName = "Battle Versatility"
    , cardColor = Red
    , cardBasic = [ [] --> replicate 2 (Attack Melee Physycal)
                  , [] --> replicate 2 (Block Physycal)
                  , [] --> [ Attack Ranged Physycal ]
                  ]
    , cardPower = [ [] --> replicate 4 (Attack Melee Physycal)
                  , [] --> replicate 4 (Block Physycal)
                  , [] --> replicate 3 (Attack Melee Fire)
                  , [] --> replicate 3 (Block Fire)
                  , [] --> replicate 3 (Attack Ranged Physycal)
                  , [] --> replicate 2 (Attack Siege Physycal)
                  ]
    }

  , Card
    { cardName = "Improvisation"
    , cardColor = Red
    , cardBasic = concatMap (improvise 3) cards
    , cardPower = concatMap (improvise 5) cards
    }

  , Card
    { cardName = "Rage"
    , cardColor = Red
    , cardBasic = [ [] --> replicate 2 (Attack Melee Physycal)
                  , [] --> replicate 2 (Block Physycal)
                  ]
    , cardPower = [ [] --> replicate 4 (Attack Melee Physycal) ]
    }

  , Card
    { cardName = "Threaten"
    , cardColor = Red
    , cardBasic = [ [] --> replicate 2 Influence ]
    , cardPower = [ [] --> ReputationLoss : replicate 5 Influence ]
    }



  , Card
    { cardName = "Mana Draw"
    , cardColor = White
    , cardBasic = [ [] --> [ ManaDie ] ]
    , cardPower = [ [ ManaSource m ] -->
                              (ManaSourceFixed n : replicate 3 (ManaToken n))
                  | m <- anyMana, n <- filter (/= Gold) anyMana
                  ]
    }

  , Card
    { cardName  = "Noble Manners"
    , cardColor = White
    , cardBasic = [ [] --> FameGainIfInteract : replicate 2 Influence ]
    , cardPower = [ [] --> FameGainIfInteract : ReputationGainIfInteract
                         : replicate 4 Influence ]
    }

  , Card
      { cardName = "Promise"
      , cardColor = White
      , cardBasic = [ [] --> replicate 2 Influence ]
      , cardPower = [ [] --> replicate 4 Influence ]
      }

  , Card
      { cardName = "Swiftness"
      , cardColor = White
      , cardBasic = [ [] --> replicate 2 Movement ]
      , cardPower = [ [] --> replicate 3 (Attack Ranged Physycal) ]
      }

  ]


blueActions :: [Card]
blueActions =
  [ Card
      { cardName = "Crystal Mastery"
      , cardColor = Blue
      , cardBasic = [ [ ManaCrystal b ] --> [ ManaCrystal b, ManaCrystal b ]
                    | b <- anyBasicMana
                    ]
      , cardPower = [ [] --> [ RegainUsedCrystals ] ]
      }

  , Card
      { cardName = "Ice Bolt"
      , cardColor = Blue
      , cardBasic = [ [] --> [ ManaCrystal Blue ] ]
      , cardPower = [ [] --> replicate 3 (Attack Ranged Ice) ]
      }

  ]



improvise :: Int -> Card -> [ Rule ]
improvise amt Card { .. } =
  [ [ ACard cardName ] --> replicate amt act
  | act <- [ Movement, Influence, Attack Melee Physycal, Block Physycal]
  ]

concentrate :: Int -> Card -> [ Rule ]
concentrate amt Card { .. }
  | cardName `elem` [ "Concentration", "Will Focus" ] = []
  | otherwise =
    [ ACard cardName : bagToList ruleIn -->
      flatGrouped [ (x, if affected x then a + amt else a)
                  | (x,a) <- bagToListGrouped ruleOut
                  ]
    | Rule { .. } <- cardPower
    ]
  where
  affected a = case a of
                 Movement  -> True
                 Influence -> True
                 Attack {} -> True
                 Block {}  -> True
                 _         -> False


-- XXX
advancedActionCards :: [Card]
advancedActionCards = []

-- XXX
spellCards :: [Card]
spellCards = []
