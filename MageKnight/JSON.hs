{-# LANGUAGE OverloadedStrings, RecordWildCards, FlexibleInstances #-}
module MageKnight.JSON where

import MageKnight.Game
import MageKnight.Common
import MageKnight.Offers
import MageKnight.Cards
import MageKnight.Units
import MageKnight.Terrain hiding (tile)
import MageKnight.GameTile
import MageKnight.HexContent
import MageKnight.Enemies
import MageKnight.Player
import MageKnight.Ruins hiding (ruins)
import MageKnight.Bag

import           Data.Text(Text)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS

class Export a where
  toJS :: a -> JS.Value

(.=) :: Export a => Text -> a -> JS.Pair
x .= y = x JS..= toJS y

jsonBytes :: Export a => a -> LBS.ByteString
jsonBytes = JS.encode . toJS

instance Export Int where
  toJS n = JS.Number (fromIntegral n)

instance Export Text where
  toJS xs = JS.String xs

instance Export JS.Value where
  toJS x = x

instance Export a => Export (Maybe a) where
  toJS x = case x of
             Nothing -> JS.Null
             Just a  -> toJS a

instance Export a => Export [a] where
  toJS xs = JS.Array (Vector.fromList (map toJS xs))

--------------------------------------------------------------------------------


instance Export Time where
  toJS t =
    case t of
      Day   -> JS.String "day"
      Night -> JS.String "night"

instance Export HexContent where
  toJS HexContent { .. } =
    JS.object [ "shields" .= reverse hexShields
              , "enemies" .= fmap enemy      (bagToList hexEnemies)
              , "players" .= fmap playerName (Set.toList hexPlayers)
              , "ruins"   .= fmap ruins      hexRuins
              ]
      where
      jsEnemy ty name = JS.object [ "type" .= toJS ty, "name" .= name ]

      enemy (v,e) = jsEnemy (enemyType e)
                  $ case v of
                      Hidden   -> "back"
                      Revealed -> enemyName e

      ruins (v,r) = case v of
                      Hidden   -> "back"
                      Revealed -> ruinsName r

instance Export EnemyType where
  toJS et = toJS (txt :: Text)
    where
    txt = case et of
            Orc         -> "orc"
            Guardian    -> "guardian"
            Mage        -> "mage"
            Underworld  -> "underworld"
            Citizen     -> "citizens"
            Draconum    -> "draconum"

instance Export BasicMana where
  toJS m = toJS (txt :: Text)
    where
    txt = case m of
            White -> "white"
            Red   -> "red"
            Blue  -> "blue"
            Green -> "green"

instance Export Mana where
  toJS m = case m of
             Gold        -> toJS ("gold" :: Text)
             Black       -> toJS ("black" :: Text)
             BasicMana b -> toJS b

instance Export MapShape where
  toJS sh = case sh of
              Wedge           -> JS.object [ "shape" .= ("wedge" :: Text) ]
              OpenMap up down -> JS.object [ "shape" .= ("open" :: Text)
                                           , "up"    .= up
                                           , "down"  .= down
                                           ]

instance Export Dir where
  toJS dir = toJS (txt :: Text)
    where
    txt = case dir of
            NE -> "NE"
            E  -> "E"
            SE -> "SE"
            SW -> "SW"
            W  -> "W"
            NW -> "NW"


instance Export HexAddr where
  toJS addr = case addr of
                Center   -> toJS ("C" :: Text)
                Border b -> toJS b


instance Export TileType where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            BasicTile -> "basic"
            CoreTile  -> "core"


instance Export Tile where
  toJS Tile { .. } = JS.object [ "name" .= tileName, "type" .= tileType ]


instance Export GameTile where
  toJS GameTile { .. } =
    JS.object [ "tile"    .= gameTile
              , "content" .= map export (Map.toList gameTileContent)
              ]
    where
    export (l,c) = JS.object [ "location" .= l, "content"  .= c ]

instance Export Card where
  toJS Card { .. } = toJS cardName

instance Export Unit where
  toJS Unit { .. } = JS.object [ "name" .= unitName, "type" .= unitType ]

instance Export UnitType where
  toJS t = toJS (txt :: Text)
    where
    txt = case t of
            RegularUnit -> "regular"
            EliteUnit   -> "elite"


instance Export Offers where
  toJS o =
    JS.object
      [ "advancedActions" .= offeringAdvancedActions o
      , "spells"          .= offeringSpells o
      , "units"           .= offeringUnits o
      , "monasteries"     .= offeringMonasteries o
      ]

instance Export Game where
  toJS Game { .. } =
    JS.object
      [ "time"      .= gameTime
      , "source"    .= bagToList theSource
      , "offers"    .= offers
      , "mapShape"  .= mapShape
      , "map"       .= map tile (Map.toList theMap)
      , "nextTiles" .= map tileType (unexploredTiles ++ backupTiles)
      ]

    where
    tile ((x,y),t) = JS.object [ "x" .= x, "y" .= y, "tile" .= t ]



