{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Units where

import MageKnight.JSON

import Data.Text (Text)


data UnitType = RegularUnit | EliteUnit

-- XXX: finish up
data Unit = Unit { unitName :: Text
                 , unitType :: UnitType
                 }

instance Export Unit where
  toJS Unit { .. } = object [ "name" .= unitName, "type" .= unitType ]

instance Export UnitType where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            RegularUnit -> "regular"
            EliteUnit   -> "elite"



-- XXX
regularUnits :: [Unit]
regularUnits = []

-- XXX
eliteUnits :: [Unit]
eliteUnits = []

