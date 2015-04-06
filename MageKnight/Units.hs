{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Units where

import MageKnight.JSON
import MageKnight.Rule
import MageKnight.Common

import           Data.Text (Text)
import           Data.Set  (Set)
import qualified Data.Set as Set
import           Data.Maybe (listToMaybe)


data UnitType = RegularUnit | EliteUnit

-- XXX: finish up
data Unit = Unit { unitName       :: Text
                 , unitType       :: UnitType
                 , unitLevel      :: Int
                 , unitCost       :: Int
                 , unitArmor      :: Int
                 , unitResists    :: Set Element
                 , unitSource     :: Set UnitSource
                 , unitAbilities  :: [Rule]
                 }

data UnitSource =
    FromVillage
  | FromMonastery
  | FromKeep
  | FromMageTower
  | FromCity
    deriving (Eq,Ord,Show)


instance Export Unit where
  toJS Unit { .. } = toJS unitName

instance Export UnitType where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            RegularUnit -> "regular"
            EliteUnit   -> "elite"

findUnit :: Text -> Maybe Unit
findUnit x =
  listToMaybe [ u | u <- regularUnits ++ eliteUnits, unitName u == x ]


regularUnits :: [Unit]
regularUnits =
  [ Unit { unitName       = "Peasants"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 4
         , unitArmor      = 3
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage ]
         , unitAbilities  =
            [ [] --> replicate 2 (Attack Melee Physycal)
            , [] --> replicate 2 (Block Physycal)
            , [] --> replicate 2 Influence
            , [] --> replicate 2 Movement
            ]
         }

  , Unit { unitName       = "Foresters"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 5
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.singleton FromVillage
         , unitAbilities  =
            [ [] --> replicate 2 Movement ++
                     [ ChangeTerrainCost t (DecreaseBy 1 0)
                                        | t <- [ Forest, Hills, Swamp ] ]
            , [] --> replicate 3 (Block Physycal)
            ]
         }

  , Unit { unitName       = "Herbalists"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 3
         , unitArmor      = 2
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromMonastery ]
         , unitAbilities  =
            [ [ ManaToken (BasicMana Green) ] --> replicate 2 Healing
            , [] --> [ ReadyUnit 1 ]
            , [] --> [ ReadyUnit 2 ]
            , [] --> [ ManaToken (BasicMana Green) ]
            ]
         }




  , Unit { unitName       = "Northern Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ [] --> replicate 3 (Attack Melee Physycal)
            , [] --> replicate 3 (Block Physycal)
            , [ ManaToken (BasicMana Blue) ] --> replicate 4 (Attack Melee Ice)
            , [ ManaToken (BasicMana Blue) ] --> replicate 4 (Block Ice)
            ]
         }

  , Unit { unitName       = "Red Cape Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ [] --> replicate 3 (Attack Melee Physycal)
            , [] --> replicate 3 (Block Physycal)
            , [ ManaToken (BasicMana Red) ] --> replicate 4 (Attack Melee Fire)
            , [ ManaToken (BasicMana Red) ] --> replicate 4 (Block Fire)
            ]
         }

  , Unit { unitName       = "Savage Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ [] --> replicate 3 (Attack Melee Physycal)
            , [] --> replicate 3 (Block Physycal)
            , [ ManaToken (BasicMana Green) ] -->
                                            replicate 4 (Attack Siege Physycal)
            ]
         }


  , Unit { unitName       = "Utem Crossbowmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ [] --> replicate 3 (Attack Melee Physycal)
            , [] --> replicate 3 (Block Physycal)
            , [] --> replicate 2 (Attack Ranged Physycal)
            ]
         }

  , Unit { unitName       = "Utem Guardsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 5
         , unitArmor      = 5
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ [] --> replicate 2 (Attack Melee Physycal)
            , [] --> replicate 4 (Block Physycal) ++ []
                      -- XXX: An enemy blocked this way looses swiftness
            ]
         }

  , Unit { unitName       = "Utem Swordsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep ]
         , unitAbilities  =
            [ [] --> replicate 3 (Attack Melee Physycal)
            , [] --> replicate 3 (Block Physycal)
            -- XXX: Attack 6, become wounded
            -- XXX: Block 6, become wounded
            ]
         }

  , Unit { unitName       = "Guardian Golems"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 3
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  =
            [ [] --> replicate 2 (Attack Melee Physycal)
            , [] --> replicate 2 (Block Physycal)
            , [ ManaToken (BasicMana Red ) ] --> replicate 4 (Block Fire)
            , [ ManaToken (BasicMana Blue) ] --> replicate 4 (Block Ice)
            ]
         }

  , Unit { unitName       = "Illusionists"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 2
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  =
            [ [] --> replicate 4 Influence
-- XXX
--            , [ ManaToken (BasicMana White) ] --> 
                  -- Target unfortified enemy does not attack
            , [] --> [ ManaToken (BasicMana White) ]
            ]
         }



  ]

-- XXX
eliteUnits :: [Unit]
eliteUnits =
  [ Unit { unitName       = "Altem Guardians"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 11
         , unitArmor      = 7
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Altem Mages"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 12
         , unitArmor      = 5
         , unitResists    = Set.fromList [ Fire, Ice ]
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Amotep Freezers"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Amotep Gunners"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Catapults"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Fire Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Fire Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Ice Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , Unit { unitName       = "Ice Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }





  ]




