-- | Locations of resources on the file system.
module Paths where

import           Data.Char(toLower,isAscii,isAlphaNum)
import           Data.List(nub)
import           Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Set as Set
import           System.FilePath((</>),(<.>))

import Common( BasicMana(..), Element(..) )
import Terrain(Feature(..))
import Deed(Deed(..),DeedType(..))
import Units(Unit(..),UnitType(..))
import Enemies


-- | Top-level directory for resources.
resourceDir :: FilePath
resourceDir = "ui"

-- | Convert the name of something, to a format suitable for file-system part.
nameToPath :: Text -> String
nameToPath = Text.unpack . Text.map cvt
  where
  cvt c | isAscii c && isAlphaNum c = toLower c
  cvt _                             = '_'

-- | Convert a URL to a path.  Removes the leading and trailing slashes.
urlToPath :: String -> FilePath
urlToPath = (resourceDir </>) . foldr (</>) "" . split
  where
  split xs = case break (== '/') xs of
               (as, _ : bs) -> as : split bs
               (as,_)       -> [as]

-- | URL for image to a specific deed.
deedUrl :: Deed -> FilePath
deedUrl Deed { .. } = dir </> nameToPath deedName <.> "png"
  where
  pref    = "img" </> "cards"
  dir     = case deedType of
              Wound            -> pref
              Action c         -> pref </> "basic_actions"    </> color c
              AdvancedAction c -> pref </> "advanced_actions" </> color c
              Spell c          -> pref </> "spells"           </> color c
              Artifact         -> pref </> "artifacts"
  color c = case c of
              Red   -> "red"
              Blue  -> "blue"
              Green -> "green"
              White -> "white"

-- | URL for image to a unit
unitUrl :: Unit -> String
unitUrl Unit { .. } = "img" </> "units" </> ty </> name <.> "png"
  where ty = case unitType of
               RegularUnit -> "regular"
               EliteUnit   -> "elite"
        name = nameToPath unitName


-- | URL for path to help-image for a feature.
featureHelpUrl :: Feature -> Maybe String
featureHelpUrl f =
  case f of
    MagicalGlade    -> img "magical_glade"
    Mine _          -> img "mines"
    Village         -> img "village"
    Monastery       -> img "monastery"
    Keep            -> img "keep"
    MageTower       -> img "mage_tower"
    City color      -> img ("city_" ++ case color of
                                         Red   -> "red"
                                         Blue  -> "blue"
                                         Green -> "green"
                                         White -> "white")
    Dungeon         -> img "dungeon"
    Tomb            -> img "tomb"
    MonsterDen      -> img "monster_den"
    SpawningGrounds -> img "spawning_grounds"
    AncientRuins    -> img "ancient_ruins"
    RampagingEnemy e ->
      case e of
        Orc      -> img "marauding_orcs"
        Draconum -> img "draconum"
        _        -> Nothing
  where
  img x = Just ("img" </> "manual" </> "features" </> x <.> "png")


-- | Help URLS to images relevant to help about this enemy.
enemyPowerHelpUrl :: Enemy -> [FilePath]
enemyPowerHelpUrl Enemy { .. } =
  coldFireDefense ++
    concatMap defenseInfo (Set.toList enemyAbilities) ++
    nub (concatMap attackInfo enemyAttack)
  where
  attackInfo x =
    case x of
      AttacksWith el _ ->
       case el of
         Physycal      -> []
         Fire          -> [ url "fire_attack" ]
         Ice           -> [ url "ice_attack" ]
         ColdFire      -> [ url "cold_fire_attack" ]

      Summoner         -> [ url "summon_attack" ]

  coldFireDefense
    | Resists Fire `Set.member` enemyAbilities &&
      Resists Ice  `Set.member` enemyAbilities =
      [ url "fire_and_ice_resistance" ]
    | otherwise = []

  defenseInfo x = case x of
                    Fortified     -> [ url "fortified" ]
                    Resists e ->
                      case e of
                        Fire        -> [ url "fire_resistance" ]
                        Ice         -> [ url "ice_resistance" ]
                        Physycal    -> [ url "physycal_resistance" ]
                        ColdFire    -> []
                    Swift         -> [ url "swift" ]
                    Brutal        -> [ url "brutal" ]
                    Poisons       -> [ url "poison" ]
                    Paralyzes     -> [ url "paralyze" ]

  url x = "img" </> "manual" </> "enemy_abilities" </> x <.> "png"


