module Units where

import Rule
import Common

import Util.JSON
import Util.Perhaps

import           Data.Text (Text)
import           Data.Set  (Set)
import qualified Data.Set as Set
import           Data.Maybe (listToMaybe)


-- | A unit hired by a player.
data ActiveUnit = ActiveUnit
  { baseUnit    :: Unit
  , unitReady   :: Bool
  , unitWounds  :: Int
  , unitAssignedDamageThisCombat :: Bool
  }

-- | Make a new active, when freshly hired from somewhere.
activeateUnit :: Unit -> ActiveUnit
activeateUnit u = ActiveUnit
  { baseUnit = u
  , unitReady = True
  , unitWounds = 0
  , unitAssignedDamageThisCombat = False
  }

-- | Make any updates necessary at the end of a combat.
endOfCombat :: ActiveUnit -> ActiveUnit
endOfCombat u = u { unitAssignedDamageThisCombat = False }

-- | Assign some type of damage to a unit.
unitAssignDamage :: Int {-^ Amount -} ->
                    DamageInfo ->
                    ActiveUnit  {-^ Unit to be damaged -} ->
                    Perhaps (Int, Maybe ActiveUnit)
                    -- ^ Remaining damage and updated unit.
                    -- If the unit is paralized, we return 'Nothing'.
unitAssignDamage damage DamageInfo { .. } ActiveUnit { .. }

  | damage < 1 =
    Failed "Units may be assigned only positive amount of damage."

  | unitWounds > 0 =
    Failed "The unit is already wounded."

  | unitAssignedDamageThisCombat =
    Failed "The unit was already assigned damage this combat."

  | otherwise =
    let resistant = damageElement `Set.member` unitResists baseUnit
        armor     = unitArmor baseUnit
        absorbed  = if resistant then 2 * armor else armor
        wounds    = if damagePoisons then 2 else 1
        actualWounds = if resistant && damage <= armor then 0 else wounds
    in Ok ( max 0 (damage - absorbed)
          , if actualWounds > 0 && damageParalyzes
              then Nothing
              else Just ActiveUnit { unitAssignedDamageThisCombat = True
                                   , unitWounds = actualWounds, .. }
          )

data UnitType = RegularUnit | EliteUnit

-- | A unit that has not been hired (i.e., in the offer)
data Unit = Unit { unitName       :: Text
                 , unitType       :: UnitType
                 , unitLevel      :: Int
                 , unitCost       :: Int
                 , unitArmor      :: Int
                 , unitResists    :: Set Element
                 , unitSource     :: Set UnitSource
                 , unitAbilities  :: [Rule]  -- XXX: finish up
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

instance Export ActiveUnit where
  toJS ActiveUnit { .. } =
    object [ "unit"   .= baseUnit
           , "ready"  .= unitReady
           , "wounds" .= unitWounds
           ]


findUnit :: Text -> Maybe Unit
findUnit x =
  listToMaybe [ u | u <- regularUnits ++ eliteUnits, unitName u == x ]


regularUnits :: [Unit]
regularUnits = []
{-
  concat
  [ replicate 3 $
    Unit { unitName       = "Peasants"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 4
         , unitArmor      = 3
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (2 *** Block Physycal)
            , produces (2 *** Influence)
            , produces (2 *** Movement)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Foresters"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 5
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.singleton FromVillage
         , unitAbilities  =
            [ produces (2 *** Movement) &&&
              produces [ ChangeTerrainCost t (DecreaseBy 1 0)
                                          | t <- [ Forest, Hills, Swamp ] ]
            , produces (3 *** Block Physycal)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Herbalists"
         , unitType       = RegularUnit
         , unitLevel      = 1
         , unitCost       = 3
         , unitArmor      = 2
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromMonastery ]
         , unitAbilities  =
            [ ManaToken (BasicMana Green) --> 2 *** Healing
            , produces (2 *** ReadyUnit 1)
            , produces (2 *** ReadyUnit 2)
            , produces (2 *** ManaToken (BasicMana Green))
            ]
         }




  , replicate 1 $
    Unit { unitName       = "Northern Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , ManaToken (BasicMana Blue) --> 4 *** Attack Melee Ice
            , ManaToken (BasicMana Blue) --> 4 *** Block Ice
            ]
         }

  , replicate 1 $
    Unit { unitName       = "Red Cape Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , ManaToken (BasicMana Red) --> 4 *** Attack Melee Fire
            , ManaToken (BasicMana Red) --> 4 *** Block Fire
            ]
         }

  , replicate 1 $
    Unit { unitName       = "Savage Monks"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromMonastery ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , ManaToken (BasicMana Green) --> 4 *** Attack Siege Physycal
            ]
         }


  , replicate 2 $
    Unit { unitName       = "Utem Crossbowmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            , produces (2 *** Attack Ranged Physycal)
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Utem Guardsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 5
         , unitArmor      = 5
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromVillage, FromKeep ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (4 *** Block Physycal) {-++ []-}
                      -- XXX: An enemy blocked this way looses swiftness
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Utem Swordsmen"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 6
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep ]
         , unitAbilities  =
            [ produces (3 *** Attack Melee Physycal)
            , produces (3 *** Block Physycal)
            -- XXX: Attack 6, become wounded
            -- XXX: Block 6, become wounded
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Guardian Golems"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 3
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  =
            [ produces (2 *** Attack Melee Physycal)
            , produces (2 *** Block Physycal)
            , ManaToken (BasicMana Red ) --> 4 *** Block Fire
            , ManaToken (BasicMana Blue) --> 4 *** Block Ice
            ]
         }

  , replicate 2 $
    Unit { unitName       = "Illusionists"
         , unitType       = RegularUnit
         , unitLevel      = 2
         , unitCost       = 7
         , unitArmor      = 2
         , unitResists    = Set.singleton Physycal
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = []
         }
  ]

-}

-- XXX
eliteUnits :: [Unit]
eliteUnits = []

{-
  concat
  [ replicate 3 $
    Unit { unitName       = "Altem Guardians"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 11
         , unitArmor      = 7
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2
    $ Unit { unitName     = "Altem Mages"
         , unitType       = EliteUnit
         , unitLevel      = 4
         , unitCost       = 12
         , unitArmor      = 5
         , unitResists    = Set.fromList [ Fire, Ice ]
         , unitSource     = Set.fromList [ FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Amotep Freezers"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Amotep Gunners"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 6
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 3 $
    Unit { unitName       = "Catapults"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.empty
         , unitSource     = Set.fromList [ FromKeep, FromCity ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Fire Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Fire Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Fire ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Ice Golems"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 8
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice, Physycal ]
         , unitSource     = Set.fromList [ FromKeep, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }

  , replicate 2 $
    Unit { unitName       = "Ice Mages"
         , unitType       = EliteUnit
         , unitLevel      = 3
         , unitCost       = 9
         , unitArmor      = 4
         , unitResists    = Set.fromList [ Ice ]
         , unitSource     = Set.fromList [ FromMonastery, FromMageTower ]
         , unitAbilities  = [] -- XXX
         }
  ]

-}


