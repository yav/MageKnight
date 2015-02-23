{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module MageKnight.Terrain
  ( -- * Addressing
    Addr(..), TileAddr,

    HexAddr(..), allHexAddrs,
    Dir(..), allDirections,
    IsHexAddr(..),

    neighbour, neighboursUpTo, globalNeighbours,

    -- * Tile Description
    Tile(..),
    Terrain(..),
    Feature(..),
    TileType(..),
    terrainCostsDuring,

    -- * Tiles
    tileA, tileB, basicTiles, coreNonCityTiles, cityTiles,
    placeHolderTile,

    -- * Map Shape
    MapShape(..),
    openMap3, openMap4, openMap5,
    validPlacement
  ) where

import MageKnight.Common
import MageKnight.Enemies(EnemyType(Orc,Draconum))
import MageKnight.JSON

import           Data.Array (array, (!))
import           Data.Text (Text)
import qualified Data.Set as Set
import           Data.Map ( Map )
import qualified Data.Map as Map

type TileAddr       = (Int,Int)

data Dir            = NE | E | SE | SW | W | NW
                      deriving (Eq,Ord,Show,Enum,Bounded)

data HexAddr        = Center | Border Dir
                      deriving (Eq,Ord,Show)

data HexNeighbour   = Local HexAddr | Foreign Int Int Dir
                      deriving (Eq,Ord,Show)

data Addr           = Addr { addrGlobal :: TileAddr, addrLocal :: HexAddr }
                      deriving (Eq,Ord,Show)

data Feature        = MagicalGlade | Mine BasicMana
                    | Village | Monastery
                    | Keep | MageTower
                    | Dungeon | Tomb
                    | MonsterDen | SpawningGrounds
                    | AncientRuins
                    | RampagingEnemy EnemyType
                      deriving (Eq,Show)

data TileType       = BasicTile | CoreTile

data Tile           = Tile { tileName    :: Text
                           , tileType    :: TileType
                           , tileTerrain :: HexAddr -> (Terrain, Maybe Feature)
                           }

placeHolderTile :: TileType -> Tile
placeHolderTile ty = Tile { tileName = "placeholder"
                          , tileType = ty
                          , tileTerrain = \_ -> (Ocean, Nothing)
                          }

allDirections :: [Dir]
allDirections = [ minBound .. maxBound ]

allHexAddrs :: [HexAddr]
allHexAddrs = Center : map Border allDirections


terrainCostsDuring :: Time -> Map Terrain Int
terrainCostsDuring time = Map.fromList $
  [ (Plains, 2)
  , (Hills,  3)
  , (Forest, if time == Day then 3 else 5)
  , (Wasteland, 4)
  , (Desert, if time == Day then 5 else 3)
  , (Swamp, 5)
  ] ++
  [ (City m, 2) | m <- anyBasicMana ]



-- | Locations that are up to the given distance.
neighboursUpTo :: Int -> Addr -> [ Addr ]
neighboursUpTo n a = Set.toList (iterate step (Set.singleton a) !! n)
  where
  neighbours x = Set.fromList [ neighbour x d | d <- allDirections ]
  step s       = Set.unions (map neighbours (Set.toList s))


-- | Location of hex one move in the given direction.
neighbour :: Addr -> Dir -> Addr
neighbour Addr { .. } dir =
  case tileGraph addrLocal dir of
    Local x         -> Addr { addrLocal = x, .. }
    Foreign dx dy a -> Addr { addrGlobal = ( fst addrGlobal + dx
                                           , snd addrGlobal + dy
                                           )
                            , addrLocal = Border a
                            }

-- | 'E' is in the direction of the increasing @x@ coordinate
globalDelta :: Dir -> TileAddr
globalDelta dir =
  case dir of
    E  -> (1,0)
    W  -> (-1,0)
    NE -> (0,1)
    NW -> (-1,1)
    SE -> (1,-1)
    SW -> (0,-1)

globalNeighbours :: TileAddr -> [TileAddr]
globalNeighbours (x,y) =
  [ (x+dx,y+dy) | (dx,dy) <- map globalDelta allDirections ]


tileGraph :: HexAddr -> Dir -> HexNeighbour
tileGraph a d =
  case a of
    Center -> local d
    Border b ->
      case b of
        NW -> case d of
                NW -> Foreign 0 1 SW
                NE -> Foreign 0 1 SE
                E  -> local NE
                SE -> local Center
                SW -> local W
                W  -> Foreign (-1) 1 E
        NE -> case d of
                NW -> Foreign 0 1 SE
                NE -> Foreign 1 0 W
                E  -> Foreign 1 0 SW
                SE -> local E
                SW -> local Center
                W  -> local NW
        E -> case d of
               NW -> local NE
               NE -> Foreign 1 0 SW
               E  -> Foreign 1 (-1) NW
               SE -> Foreign 1 (-1) W
               SW -> local SE
               W  -> local Center
        SE -> case d of
                NW -> local Center
                NE -> local E
                E  -> Foreign 1 (-1) W
                SE -> Foreign 0 (-1) NE
                SW -> Foreign 0 (-1) NW
                W  -> local SW
        SW -> case d of
                NW -> local W
                NE -> local Center
                E  -> local SE
                SE -> Foreign 0 (-1) NW
                SW -> Foreign (-1) 0 E
                W  -> Foreign (-1) 0 NE
        W -> case d of
               NW -> Foreign (-1) 1 E
               NE -> local NW
               E  -> local Center
               SE -> local SW
               SW -> Foreign (-1) 0 NE
               W  -> Foreign (-1) 1 SE

  where local x = Local (hexAddr x)


--------------------------------------------------------------------------------

data MapShape = Wedge | OpenMap Int Int

openMap3 :: MapShape
openMap3 = OpenMap 1 (-1)

openMap4 :: MapShape
openMap4 = OpenMap 1 (-2)

openMap5 :: MapShape
openMap5 = OpenMap 2 (-2)

isOnMap :: MapShape -> TileAddr -> Bool
isOnMap sh (x,y) =
  case sh of
    Wedge -> x >= 0 && y >= 0
    OpenMap up down -> x >= 0 && y <= up && down <= y && (y >= 0 || -x <= y)

-- | Assuming the position is on the map.
isCoastal :: MapShape -> TileAddr -> Bool
isCoastal sh (x,y) =
  case sh of
    Wedge           -> x == 0 || y == 0
    OpenMap up down -> y == up || y == down || (-x == y)

validPlacement :: MapShape -> (TileAddr -> Bool) -> TileType -> Bool ->
                  TileAddr -> Bool
validPlacement sh explored t backup pt =
  isOnMap sh pt && not (explored pt) &&
  (case (t, neighboursOf pt) of
     (BasicTile, _ : _ : xs) -> backupCheck xs
     (BasicTile, [b]) | not backup -> case neighboursOf b of
                                        _ : _ : _ -> True
                                        _         -> False
     (CoreTile, _ : _ : xs)  -> not (isCoastal sh pt) && backupCheck xs
     _                       -> False)
  where
  backupCheck xs  = not backup || not (null xs)
  neighboursOf a  = [ b | b <- globalNeighbours a, explored b ]



--------------------------------------------------------------------------------

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


instance Export MapShape where
  toJS sh = case sh of
              Wedge           -> object [ "shape" .= ("wedge" :: Text) ]
              OpenMap up down -> object [ "shape" .= ("open" :: Text)
                                        , "up"    .= up
                                        , "down"  .= down
                                        ]

instance Export Addr where
  toJS Addr { addrGlobal = (x,y), .. } = object [ "x"   .= x
                                                , "y"   .= y
                                                , "hex" .= addrLocal
                                                ]


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
  toJS Tile { .. } = object [ "name" .= tileName, "type" .= tileType ]




--------------------------------------------------------------------------------

class IsHexAddr a where
  hexAddr :: a -> HexAddr

instance IsHexAddr Dir where
  hexAddr = Border

instance IsHexAddr HexAddr where
  hexAddr = id



class IsHexContent a where
  hexContent :: a -> (Terrain, Maybe Feature)

instance IsHexContent Terrain where
  hexContent a = (a, Nothing)

instance IsHexContent (Terrain,Feature) where
  hexContent (a,b) = (a,Just b)

instance IsHexContent (Terrain,EnemyType) where
  hexContent (a,b) = (a,Just (RampagingEnemy b))

type Hex = (HexAddr, (Terrain, Maybe Feature))

listTileFun :: [Hex] -> HexAddr -> (Terrain, Maybe Feature)
listTileFun xs = \d -> arr ! cvt d
  where
  arr   = array (0,1 + fromEnum (maxBound :: Dir)) [ (cvt c,a) | (c,a) <- xs ]
  cvt c = case c of
            Center   -> 0
            Border b -> 1 + fromEnum b

(|->) :: (IsHexAddr a, IsHexContent b) => a -> b -> Hex
a |-> b = (hexAddr a, hexContent b)

tile :: Text -> TileType -> [Hex] -> Tile
tile tileName tileType hexes = Tile { tileTerrain = listTileFun hexes, .. }


tileA :: Tile
tileA = tile "A" BasicTile [ NW     |-> Plains
                           , NE     |-> Forest
                           , W      |-> Ocean
                           , Center |-> Plains -- and portal...
                           , E      |-> Plains
                           , SW     |-> Ocean
                           , SE     |-> Ocean
                           ]

tileB :: Tile
tileB = tile "B" BasicTile [ NW     |-> Plains
                           , NE     |-> Forest
                           , W      |-> Ocean
                           , Center |-> Plains -- and portal...
                           , E      |-> Plains
                           , SW     |-> Ocean
                           , SE     |-> Plains
                           ]

basicTiles :: [Tile]
basicTiles = map basic
  [ ("1", [ NW     |-> (Forest, Orc)
          , NE     |-> Lake
          , W      |-> Forest
          , Center |-> (Forest, MagicalGlade)
          , E      |-> (Plains, Village)
          , SW     |-> Plains
          , SE     |-> Plains
          ])

  , ("2", [ NW     |-> (Hills, Orc)
          , NE     |-> (Forest, MagicalGlade)
          , W      |-> Plains
          , Center |-> Hills
          , E      |-> (Plains, Village)
          , SW     |-> (Hills, Mine Green)
          , SE     |-> Plains
          ])

  , ("3", [ NW     |-> Plains
          , NE     |-> (Hills, Keep)
          , W      |-> Plains
          , Center |-> Forest
          , E      |-> Hills
          , SW     |-> (Plains, Village)
          , SE     |-> (Hills, Mine White)
          ])

  , ("4", [ NW     |-> Desert
          , NE     |-> Desert
          , W      |-> (Hills, Orc)
          , Center |-> (Desert, MageTower)
          , E      |-> Mountain
          , SW     |-> Plains
          , SE     |-> (Plains, Village)
          ])

  , ("5", [ NW     |-> Forest
          , NE     |-> (Plains, Monastery)
          , W      |-> (Forest, MagicalGlade)
          , Center |-> Lake
          , E      |-> (Plains, Orc)
          , SW     |-> Forest
          , SE     |-> (Hills, Mine Blue)
          ])

  , ("6", [ NW     |-> Mountain
          , NE     |-> Forest
          , W      |-> (Hills, MonsterDen)
          , Center |-> (Hills, Mine Red)
          , E      |-> Plains
          , SW     |-> Hills
          , SE     |-> (Forest, Orc)
          ])

  , ("7", [ NW     |-> Lake
          , NE     |-> (Forest, Orc)
          , W      |-> (Plains, Monastery)
          , Center |-> Swamp
          , E      |-> (Forest, MagicalGlade)
          , SW     |-> Plains
          , SE     |-> (Plains, Dungeon)
          ])

  , ("8", [ NW     |-> (Forest, MagicalGlade)
          , NE     |-> (Forest, AncientRuins)
          , W      |-> Forest
          , Center |-> (Swamp, Orc)
          , E      |-> Plains
          , SW     |-> Swamp
          , SE     |-> (Swamp, Village)
          ])

  , ("9", [ NW     |-> (Wasteland, Dungeon)
          , NE     |-> Mountain
          , W      |-> Plains
          , Center |-> Mountain
          , E      |-> (Wasteland, Keep)
          , SW     |-> (Wasteland, MageTower)
          , SE     |-> Plains
          ])

  , ("10",[ NW     |-> (Hills, MonsterDen)
          , NE     |-> Forest
          , W      |-> Hills
          , Center |-> Mountain
          , E      |-> Plains
          , SW     |-> (Hills, Keep)
          , SE     |-> (Hills, AncientRuins)
          ])

  , ("11",[ NW     |-> Hills
          , NE     |-> Lake
          , W      |-> (Plains, AncientRuins)
          , Center |-> (Plains, MageTower)
          , E      |-> Lake
          , SW     |-> Lake
          , SE     |-> (Hills, Orc)
          ])
  ]
  where
  basic (x,y) = tile x BasicTile y



coreNonCityTiles :: [Tile]
coreNonCityTiles = map core
  [ ("1", [ NW     |-> Mountain
          , NE     |-> (Desert, Tomb)
          , W      |-> (Hills, SpawningGrounds)
          , Center |-> (Desert, Monastery)
          , E      |-> Desert
          , SW     |-> Hills
          , SE     |-> Desert
          ])

  , ("2", [ NW     |-> Lake
          , NE     |-> (Swamp, AncientRuins)
          , W      |-> Forest
          , Center |-> Lake
          , E      |-> (Hills, Mine Green)
          , SW     |-> (Swamp, MageTower)
          , SE     |-> (Swamp, Draconum)
          ])

  , ("3", [ NW     |-> Mountain
          , NE     |-> (Wasteland, AncientRuins)
          , W      |-> (Wasteland, Tomb)
          , Center |-> Wasteland
          , E      |-> (Hills, MageTower)
          , SW     |-> (Hills, Mine White)
          , SE     |-> Wasteland
          ])

  , ("4", [ NW     |-> (Hills, Mine Blue)
          , NE     |-> Hills
          , W      |-> Wasteland
          , Center |-> (Mountain, Draconum)
          , E      |-> (Wasteland, Keep)
          , SW     |-> (Wasteland, AncientRuins)
          , SE     |-> Wasteland
          ])
  ]
  where
  core (x,y) = tile x CoreTile y


cityTiles :: [Tile]
cityTiles = map core
  [ ("5", [ NW     |-> (Forest, MagicalGlade)
          , NE     |-> (Swamp, Village)
          , W      |-> Lake
          , Center |-> City Green
          , E      |-> (Swamp, Orc)
          , SW     |-> (Forest, Orc)
          , SE     |-> Swamp
          ])

  , ("6", [ NW     |-> Forest
          , NE     |-> (Plains, Monastery)
          , W      |-> (Mountain, Draconum)
          , Center |-> City Blue
          , E      |-> Lake
          , SW     |-> Hills
          , SE     |-> Lake
          ])

  , ("7", [ NW     |-> (Wasteland, SpawningGrounds)
          , NE     |-> Plains
          , W      |-> (Wasteland, Keep)
          , Center |-> City White
          , E      |-> Forest
          , SW     |-> Lake
          , SE     |-> (Lake, Draconum)
          ])

  , ("8", [ NW     |-> (Desert, AncientRuins)
          , NE     |-> (Hills, Mine Red)
          , W      |-> (Wasteland, Draconum)
          , Center |-> City Red
          , E      |-> Desert
          , SW     |-> Wasteland
          , SE     |-> (Desert, Draconum)
          ])

  ]
  where
  core (x,y) = tile x CoreTile y



