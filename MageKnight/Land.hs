{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Land
  ( LandSetup(..), defaultLandSetup, setupLand
  , Land
  , exploreAt
  , placePlayer
  , removePlayer
  , movePlayer
  , addrOnMap
  , setTime
  , getTime
  ) where

import           MageKnight.Terrain
import           MageKnight.HexContent
import           MageKnight.Enemies( Enemy(..), EnemyType(..)
                                   , allEnemies, allEnemyTypes )
import           MageKnight.Player
import           MageKnight.Ruins(Ruins, ruins)
import           MageKnight.Common(Time(..), Visibility(..))
import           MageKnight.Random(shuffle, split, StdGen)
import           MageKnight.ResourceQ(ResourceQ)
import qualified MageKnight.ResourceQ as RQ
import           MageKnight.JSON
import           MageKnight.Perhaps

import           Data.Map ( Map )
import qualified Data.Map as Map
import           Control.Monad(foldM)


data LandSetup = LandSetup
  { useShape      :: MapShape
  , useBasicNum   :: Int
  , useBasicTiles :: [ Tile ]
  , useCoreNum    :: Int
  , useCoreTiles  :: [ Tile ]
  , useCityLevels :: [ Int ]
  , useStartTime  :: Time
  , useEnemies    :: [ Enemy ]
  , useRuins      :: [ Ruins ]
  }

defaultLandSetup :: MapShape ->
                    Int   {- ^ Number of basic tiles -} ->
                    Int   {- ^ Number of core tiles -} ->
                    [Int] {- ^ City levels -} ->
                    LandSetup
defaultLandSetup shape basicNum coreNum cities = LandSetup
  { useShape      = shape
  , useBasicNum   = basicNum
  , useBasicTiles = basicTiles
  , useCoreNum    = coreNum
  , useCoreTiles  = coreNonCityTiles
  , useCityLevels = cities
  , useStartTime  = Day
  , useEnemies    = allEnemies
  , useRuins      = ruins
  }

-- | Setup an initial land.
-- Returns the land and the number of moansteries in the original setup.
setupLand :: StdGen -> LandSetup -> Perhaps (Land, Int)
setupLand g0 LandSetup { .. } = foldM reveal (land0,0) revealPos
  where
  land0 = Land
            { mapShape        = useShape
            , theMap          = Map.empty
            , unexploredTiles = startTile : questBasic ++ questCore
            , backupTiles     = backupBasic ++ backupCore
            , cityLevels      = useCityLevels
            , ruinsPool       = RQ.fromListRandom randRuins useRuins
            , enemyPool       = initialEnemyPool randEnemy useEnemies
            , timeOfDay       = useStartTime
            }
  (startTile,revealPos)         = case useShape of
                                    Wedge -> (tileA, init startPositions)
                                    _     -> (tileB, startPositions)
  startPositions                = [(0,0), (0,1), (1,0), (1,-1)]
  reveal (l,ms) addr            = do (l1,ms1) <- initialTile True addr l
                                     return (l1, ms + ms1)

  (shuffledBasic,g1)            = shuffle g0 useBasicTiles
  (shuffledCore, g2)            = shuffle g1 coreNonCityTiles
  (shuffledCities,g3)           = shuffle g2 cityTiles

  (questBasic,backupBasic)      = splitAt useBasicNum shuffledBasic
  (questCoreNonCity,backupCore) = splitAt useCoreNum  shuffledCore
  cityNum                       = length useCityLevels
  (questCore,g4)                = shuffle g3 (take cityNum shuffledCities ++
                                              questCoreNonCity)
  (randEnemy,randRuins)         = split g4



blankEmenyPool :: StdGen -> Map EnemyType (ResourceQ Enemy)
blankEmenyPool g0 = snd $ foldr add (g0,Map.empty) allEnemyTypes
  where
  add e (g,m) = let (g1,g2) = split g
                in (g2, Map.insert e (RQ.empty g1) m)

initialEnemyPool :: StdGen -> [Enemy] -> Map EnemyType (ResourceQ Enemy)
initialEnemyPool g0 enemies = foldr add (blankEmenyPool g0) enemies
  where
  add e qs = Map.adjust (RQ.discard e) (enemyType e) qs



data Land = Land
  { mapShape        :: MapShape
    -- ^ Basic map shape.

  , theMap          :: Map TileAddr GameTile
    -- ^ Current state of the map.

  , unexploredTiles :: [ Tile ]
    -- ^ Tiles that are part of the scenario

  , backupTiles     :: [ Tile ]
    -- ^ This is used if we run out of the scenario tiles.
    -- Basic tiles should be before core ones.

  , cityLevels      :: [Int]
    -- ^ The levels of the next cities to be reveled.

  , ruinsPool       :: ResourceQ Ruins
    -- ^ Ruins are choisen from here.

  , enemyPool       :: Map EnemyType (ResourceQ Enemy)
    -- ^ Enemies are spawned from here

  , timeOfDay       :: Time
    -- ^ Is it day or night?
  }



-- | Check if there are any more tiles available.  If so, also check
-- if the next tile may be placed at the given location.
selectTile :: Bool -> TileAddr -> Land -> Perhaps (Tile, Land)
selectTile noCheck pt Land { .. }
  | t : ts <- unexploredTiles =
    do checkThat (noCheck ||
                    validPlacement mapShape explored (tileType t) False pt)
         "The next tile does not fit the location."
       return (t, Land { unexploredTiles = ts, .. })

  | t : ts <- backupTiles =
    do checkThat (validPlacement mapShape explored (tileType t) True pt)
         "The next backup tile does not fit the location."
       return (t, Land { backupTiles = ts, .. })

  | otherwise = fail "No more map tiles."
  where
  explored a = a `Map.member` theMap


-- | Setup a newly reveal tile.
-- The 'Int' in the result is the number of monasteries on the tile.
-- This is returned so that we can add the appropriate monaetry tech to
-- the offers.
populateTile :: Tile -> Land -> (GameTile, Int, Land)
populateTile gameTile g = (GameTile { .. }, mons, g1)
  where
  (gameTileContent, mons, g1) = foldr setupHex (Map.empty,0,g) allHexAddrs

  setupHex a nothing@(c,ms,Land { .. }) =
    let addHex h  = Map.insert a h c
        enemy v t = case hexWithEnemy v t enemyPool of
                      (h,p) -> (addHex h, ms, Land { enemyPool = p, .. })

    in case tileTerrain gameTile a of

         (City color, _) | l : ls <- cityLevels ->
           case hexWithCity color l enemyPool of
             (h, p) -> (addHex h, ms,
                                  Land { enemyPool = p, cityLevels = ls, .. })

         (_, Just feature) ->
           case feature of
             MagicalGlade      -> nothing
             Mine _            -> nothing
             Village           -> nothing
             Monastery         -> (c, 1 + ms, Land { .. })
             Keep              -> enemy Hidden Guardian
             MageTower         -> enemy Hidden Mage
             Dungeon           -> nothing
             Tomb              -> nothing
             MonsterDen        -> nothing
             SpawningGrounds   -> nothing
             AncientRuins      ->
               case hexWithRuins timeOfDay ruinsPool of
                 (h,r) -> (addHex h, ms, Land { ruinsPool = r, .. })
             RampagingEnemy et -> enemy Revealed et

         _ -> nothing


-- | Check the neighbours of a tile, to see if anything should be revealed.
revealHiddenNeighbours :: Addr -> Land -> Land
revealHiddenNeighbours a Land { .. } =
  Land { theMap = foldr checkAt theMap (map (neighbour a) allDirections), .. }
  where
  checkAt Addr { .. } mp = Map.adjust (gameTileUpdateAt addrLocal check)
                                      addrGlobal mp

  check t f h = if shouldReveal t f then hexReveal h else h

  shouldReveal (City _) _         = True
  shouldReveal _ (Just Keep)      = timeOfDay == Day
  shouldReveal _ (Just MageTower) = timeOfDay == Day
  shouldReveal _ _                = False


-- | Reveal information when a player enters a location.
revealHidden :: Addr -> Land -> Land
revealHidden a l = updateAddr a upd l
  where
  upd (City _) _            = hexReveal
  upd _ (Just AncientRuins) = hexReveal
  upd _ (Just MageTower)    = hexReveal
  upd _ (Just Keep)         = hexReveal
  upd _ _                   = id


-- | Try to explore.
-- Fails of the address is explored, or there is no suitable land to put there.
exploreAt :: Addr -> TileAddr -> Land -> Perhaps (Land, Int)
exploreAt loc newTilePos l =
  do (l1,m) <- initialTile False newTilePos l
     return (revealHiddenNeighbours loc l1, m)

-- | Setup a new tile at the given position.
initialTile :: Bool -> TileAddr -> Land -> Perhaps (Land, Int)
initialTile noCheck addr l =
  do (t,l1) <- selectTile noCheck addr l
     let (gt,ms,l2) = populateTile t l1
     return (l2 { theMap = Map.insert addr gt (theMap l2) }, ms)

-- | Update a location on the map.
updateAddr :: Addr ->
              (Terrain -> Maybe Feature -> HexContent -> HexContent) ->
              Land -> Land
updateAddr Addr { .. } f Land { .. } =
  Land { theMap = Map.adjust (gameTileUpdateAt addrLocal f) addrGlobal theMap
       , .. }

-- | Place a player on the map.
placePlayer :: Player -> Land -> Land
placePlayer p = revealHidden loc
              . revealHiddenNeighbours loc
              . updateAddr loc (\_ _ -> hexAddPlayer p)
  where loc = playerLocation p

-- | Remove a player from the map.
removePlayer :: Player -> Land -> Land
removePlayer p = updateAddr (playerLocation p) (\_ _ -> hexRemovePlayer p)

-- | Move a player to the given address.  While most of the time, the address
-- will be adjacent to the player, this might not be the case if
-- "Space Bending" is activated.
movePlayer :: Player -> Addr -> Land -> (Player, Land)
movePlayer p newLoc l = (p1, placePlayer p1 l1)
  where
  l1      = removePlayer p l
  p1      = p { playerLocation = newLoc
              , playerOnUnsafe =
                  if isSafe (playerName p) newLoc l1
                    then Nothing
                    else Just $ case playerOnUnsafe p of
                                  Nothing      -> (playerLocation p, 0)
                                  Just (sl,ws) -> let ws' = ws + 1
                                                  in seq ws' (sl,ws')
              }




addrOnMap :: Addr -> Land -> Bool
addrOnMap Addr { .. } Land { .. } = addrGlobal `Map.member` theMap


-- | Set the current time for the land.
setTime :: Time -> Land -> Land
setTime t Land { .. } = Land { timeOfDay = t, .. }

-- | Is it day or night?
getTime :: Land -> Time
getTime Land { .. } = timeOfDay


-- | Is this a safe location for the given player.
isSafe :: PlayerName -> Addr -> Land -> Bool
isSafe p Addr { .. } Land { .. } =
  case Map.lookup addrGlobal theMap of
    Nothing -> False
    Just gt -> gameTileIsSafe gt addrLocal p


-- | Who would get provoked if we moved from the firt
-- provoked :: Addr -> Addr -> Land -> [Addr]

--------------------------------------------------------------------------------

data GameTile = GameTile
  { gameTile        :: Tile
  , gameTileContent :: Map HexAddr HexContent
  }

gameTileUpdateAt :: HexAddr ->
                    (Terrain -> Maybe Feature -> HexContent -> HexContent) ->
                    GameTile -> GameTile
gameTileUpdateAt a f GameTile { .. } =
  GameTile { gameTileContent =
               case Map.updateLookupWithKey upd a gameTileContent of
                 (Nothing, _)  -> Map.insert a (g hexEmpty) gameTileContent
                 (Just _, gtc) -> gtc
           , .. }
    where g = case tileTerrain gameTile a of
                (x,y) -> f x y

          upd _ h = Just (g h)

-- | Is this a safe location for the given player.
gameTileIsSafe :: GameTile -> HexAddr -> PlayerName -> Bool
gameTileIsSafe GameTile { .. } loc p =
  case tileTerrain gameTile loc of
    (City _, _)    -> False
    (Lake, _)      -> False
    (Mountain, _)  -> False
    (Ocean, _)     -> False
    (_, Nothing)   -> True
    (_, Just f)    ->
      case Map.lookup loc gameTileContent of
        Nothing  -> True
        Just hex ->
          not (hexHasPlayers hex) &&
          (case f of
             Keep             -> hexHasShield p hex
             MageTower        -> not (hexHasEnemies hex)
             RampagingEnemy _ -> not (hexHasEnemies hex)
             _                -> True)


--------------------------------------------------------------------------------

instance Export GameTile where
  toJS GameTile { .. } =
    object [ "tile"    .= gameTile
           , "content" .= map export (Map.toList gameTileContent)
           ]
    where
    export (l,c) = object [ "location" .= l, "content"  .= c ]

instance Export Land where
  toJS Land { .. } =
    object [ "time"      .= timeOfDay
           , "mapShape"  .= mapShape
           , "map"       .= map expTile (Map.toList addBounds)
           , "nextTiles" .= map tileType (unexploredTiles ++ backupTiles)
           ]
    where
    expTile ((x,y),t) = object [ "x" .= x, "y" .= y, "tile" .= t ]

    mbPlace = case (unexploredTiles, backupTiles) of
               ([],[]) -> Nothing
               (Tile { .. } : _, _) ->
                  Just (placeHolder tileType, valid tileType False)
               (_, Tile { .. } : _) ->
                  Just (placeHolder tileType, valid tileType True)

    placeHolder t = GameTile { gameTileContent = Map.empty
                             , gameTile        = placeHolderTile t
                             }

    valid = validPlacement mapShape (`Map.member` theMap)

    addBounds =
      case mbPlace of
        Nothing -> theMap
        Just (pl,isValid) -> foldr addBoundsAt theMap (Map.keys theMap)
          where
          addBoundsAt x m = foldr addBound m (globalNeighbours x)
          addBound x m
            | isValid x   = Map.insertWith (\_ old -> old) x pl m
            | otherwise   = m


