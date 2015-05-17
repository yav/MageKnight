{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module MageKnight.Land
  ( -- * Setting up the land
    Land
  , LandSetup(..), defaultLandSetup, setupLand

    -- * Exploration
  , exploreAt
  , isRevealed

    -- * Moving players
  , placePlayer
  , removePlayer
  , movePlayer
  , provoked
  , isLastMove
  , mayWalkOnto

    -- * Time
  , setTime
  , getTime

    -- * Info about tiles
  , getFeatureAt
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

import           Data.Maybe ( mapMaybe )
import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Monad( foldM, guard )


-- | General setting for setting up the map.
data LandSetup = LandSetup
  { useShape        :: MapShape
  , useCountryNum   :: Int
  , useCountryTiles :: [ Tile ]
  , useCoreNum      :: Int
  , useCoreTiles    :: [ Tile ]
  , useCityLevels   :: [ Int ]
  , useStartTime    :: Time
  , useEnemies      :: [ Enemy ]
  , useRuins        :: [ Ruins ]
  }

defaultLandSetup :: MapShape ->
                    Int   {- ^ Number of coutryside tiles -} ->
                    Int   {- ^ Number of core tiles -} ->
                    [Int] {- ^ City levels -} ->
                    LandSetup
defaultLandSetup shape countryNum coreNum cities = LandSetup
  { useShape        = shape
  , useCountryNum   = countryNum
  , useCountryTiles = countryTiles
  , useCoreNum      = coreNum
  , useCoreTiles    = coreNonCityTiles
  , useCityLevels   = cities
  , useStartTime    = Day
  , useEnemies      = allEnemies
  , useRuins        = ruins
  }

-- | Setup an initial land.
-- Returns the land and the number of moansteries in the original setup.
setupLand :: StdGen -> LandSetup -> Perhaps (Land, Int)
setupLand g0 LandSetup { .. } = foldM reveal (land0,0) revealPos
  where
  land0 = Land
            { mapShape        = useShape
            , theMap          = Map.empty
            , unexploredTiles = startTile : questCountry ++ questCore
            , backupTiles     = backupCountry ++ backupCore
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

  (shuffledCountry,g1)            = shuffle g0 useCountryTiles
  (shuffledCore, g2)            = shuffle g1 coreNonCityTiles
  (shuffledCities,g3)           = shuffle g2 cityTiles

  (questCountry,backupCountry)      = splitAt useCountryNum shuffledCountry
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
    -- Country tiles should be before core ones.

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
  during d f = if timeOfDay l == d then f else id

  upd (City _) _            = hexReveal
  upd _ (Just AncientRuins) = hexReveal
  upd _ (Just MageTower)    = during Day hexReveal
  upd _ (Just Keep)         = during Day hexReveal
  upd _ _                   = id


-- | Try to explore the given address.
-- If successful, returns an updated land, and the number of
-- monasteries that were revealed, so that the units offer can be updated.
-- Fails if the address is explored, or there is no suitable land to put there.
exploreAt :: Addr -> TileAddr -> Land -> Perhaps (Land, Int)
exploreAt loc newTilePos l =
  do (l1,m) <- initialTile False newTilePos l
     return (revealHiddenNeighbours loc l1, m)

-- | Get the feature of a tile at the given address.
getFeatureAt :: Addr -> Land -> Maybe Feature
getFeatureAt Addr { .. } Land { .. } =
  do GameTile { gameTile = Tile { .. } } <- Map.lookup addrGlobal theMap
     let (_,mbF) = tileTerrain addrLocal
     mbF

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

-- | Place a player on the map.  If during the day, reveal *adjecent*
-- relevant locations.  It does not reveal enemies *on* the location.
placePlayer :: Player -> Land -> Land
placePlayer p = revealHiddenNeighbours loc
              . revealHidden loc
              . updateAddr loc (\_ _ -> hexAddPlayer p)
  where loc = playerLocation p

-- | Remove a player from the map.
removePlayer :: Player -> Land -> Land
removePlayer p = updateAddr (playerLocation p) (\_ _ -> hexRemovePlayer p)

{- | Move a player to the given address.  Most of the time the address
will be adjacent to the player, however, this might not be the case if
"Space Bending" is activated.
Fails if the address is not on the map. -}
movePlayer :: Player -> Addr -> Land -> Perhaps (Player, Land)
movePlayer p newLoc l
  | isRevealed newLoc l = Ok (p1, placePlayer p1 l1)
  | otherwise           = Failed "This address is not on the map."
  where
  l1      = removePlayer p l
  p1      = playerSetLoc (isSafe (playerName p) newLoc l1) newLoc p


-- | Compute which addresses get provoked, if we move from one location
-- to another normally (i.e., "walking").
provoked :: Land -> Addr -> Addr -> [(Addr,[Enemy])]
provoked Land { .. } from to =
    mapMaybe hasRampaging
  $ Set.toList
  $ Set.intersection (neighboursOf from) (neighboursOf to)
  where
  neighboursOf x = Set.fromList [ neighbour x d | d <- allDirections ]
  hasRampaging a@Addr { .. } =
    do GameTile { .. }  <- Map.lookup addrGlobal theMap
       RampagingEnemy _ <- snd (tileTerrain gameTile addrLocal)
       hex <- Map.lookup addrLocal gameTileContent
       let es = hexActiveEnemies hex
       guard (not (null es))
       return (a,es)


-- | Compute if moving onto this address will end the movement phase
-- if moving normally ("walking").
isLastMove :: PlayerName -> Addr -> Land -> Bool
isLastMove p Addr { .. } Land { .. } =
  case Map.lookup addrGlobal theMap of
    Nothing -> True   -- We fell off the map?
    Just gt -> gameTileEndsMovement gt addrLocal p

-- | Can we walk into the given address?
mayWalkOnto :: Addr -> Land -> Bool
mayWalkOnto Addr { .. } Land { .. } =
  case Map.lookup addrGlobal theMap of
    Nothing -> False
    Just gt -> gameTileIsWalkable gt addrLocal


-- | Is this address revealed?
isRevealed :: Addr -> Land -> Bool
isRevealed Addr { .. } Land { .. } = addrGlobal `Map.member` theMap


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

-- | Would moving on this tile end the movement phase?
gameTileEndsMovement :: GameTile -> HexAddr -> PlayerName -> Bool
gameTileEndsMovement GameTile { .. } loc p =
  case Map.lookup loc gameTileContent of
    Nothing  -> False
    Just hex ->
      case tileTerrain gameTile loc of
        (City _, _)           -> hexHasEnemies hex
        (_, Just MageTower)   -> hexHasEnemies hex
        (_, Just Keep)        -> hexHasEnemies hex || not (hexHasShield p hex)
        (_, Just (RampagingEnemy _)) -> hexHasEnemies hex
        _                     -> False

-- | Can we walk onto this hex at all?
gameTileIsWalkable :: GameTile -> HexAddr -> Bool
gameTileIsWalkable GameTile { .. } loc =
  case tileTerrain gameTile loc of
    (Ocean, _) -> False
    (_, Just (RampagingEnemy _)) ->
      case Map.lookup loc gameTileContent of
        Just hex | hexHasEnemies hex -> False
        _                            -> True
    _ -> True




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


