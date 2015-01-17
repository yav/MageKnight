module MageKnight.Units where

import Data.Text (Text)


data UnitType = RegularUnit | EliteUnit

-- XXX: finish up
data Unit = Unit { unitName :: Text
                 , unitType :: UnitType
                 }
