{-# LANGUAGE RecordWildCards, OverloadedStrings, Safe #-}
module Land
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

    -- * Time
  , setTime
  , getTime

    -- * Info about tiles
  , getFeatureAt
  , getRevealedEnemiesAt
  , locationCardBonus

    -- * Combat
  , EnemyLifeSpan(..)
  , CombatInfo(..)
  , startCombatAt
  , summonCreature
  , discardEnemy

  ) where

import  Terrain
import  HexContent
import  GameTile
import  Enemies( Enemy(..), EnemyType(..)
               , allEnemies, allEnemyTypes )
import  Player
import  Ruins(Ruins, ruins, Objectve(..))
import  Common(Time(..), Visibility(..))

import  Util.Random
import  Util.ResourceQ
import  Util.JSON
import  Util.Perhaps

import           Data.Maybe ( mapMaybe, fromMaybe, maybeToList )
import           Data.List ( delete )
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
setupLand :: LandSetup -> Gen (Perhaps (Land, Int))
setupLand LandSetup { .. } =
  do shuffledCountry <- shuffle useCountryTiles
     shuffledCore    <- shuffle coreNonCityTiles
     shuffledCities  <- shuffle cityTiles

     let (questCountry,backupCountry)  = splitAt useCountryNum shuffledCountry
         (questCoreNonCity,backupCore) = splitAt useCoreNum    shuffledCore

     questCore <- shuffle (take cityNum shuffledCities ++ questCoreNonCity)

     ruinsPool <- rqFromListRandom useRuins
     enemyPool <- initialEnemyPool useEnemies

     let land0 = Land { mapShape        = useShape
                      , theMap          = Map.empty
                      , unexploredTiles = startTile : questCountry ++ questCore
                      , backupTiles     = backupCountry ++ backupCore
                      , cityLevels      = useCityLevels
                      , timeOfDay       = useStartTime
                      , ..
                      }
     return (foldM reveal (land0,0) revealPos)
  where
  (startTile,revealPos)         = case useShape of
                                    Wedge -> (tileA, init startPositions)
                                    _     -> (tileB, startPositions)

  startPositions                = [(0,0), (0,1), (1,0), (1,-1)]

  reveal (l,ms) addr            = do (l1,ms1) <- initialTile True addr l
                                     return (l1, ms + ms1)

  cityNum                       = length useCityLevels



blankEnemyPool :: Gen (Map EnemyType (ResourceQ Enemy))
blankEnemyPool = foldM add Map.empty allEnemyTypes
  where
  add m e = do q <- rqEmpty
               return (Map.insert e q m)


initialEnemyPool :: [Enemy] -> Gen (Map EnemyType (ResourceQ Enemy))
initialEnemyPool enemies = do blank <- blankEnemyPool
                              return (foldr add blank enemies)
  where
  add e qs = Map.adjust (rqDiscard e) (enemyType e) qs



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
populateTile tile g = (gt, mons, g1)
  where
  (gt , mons, g1) = foldr setupHex (emptyGameTile tile, 0, g) allHexAddrs

  setupHex a nothing@(c,ms,Land { .. }) =
    let addHex h  = gameTileUpdateAt a (\_ -> h) c
        enemy v t = case hexWithEnemy v t enemyPool of
                      (h,p) -> (addHex h, ms, Land { enemyPool = p, .. })

    in case tileTerrain tile a of

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

  check HexInfo { .. }
    | shouldReveal hexTerrain hexFeature = hexReveal hexContent
    | otherwise                          = hexContent

  shouldReveal (City _) _         = True
  shouldReveal _ (Just Keep)      = timeOfDay == Day
  shouldReveal _ (Just MageTower) = timeOfDay == Day
  shouldReveal _ _                = False


-- | Reveal information when a player enters a hex.
revealHidden :: Addr -> Land -> Land
revealHidden a l = updateAddr a (\h -> upd h (hexContent h)) l
  where
  during d f = if timeOfDay l == d then f else id

  upd HexInfo { .. } =
    case hexTerrain of
      City _                    -> hexReveal
      _ -> case hexFeature of
             Just AncientRuins -> hexReveal
             Just MageTower    -> during Day hexReveal
             Just Keep         -> during Day hexReveal
             _                 -> id


data EnemyLifeSpan = EnemySummoned | EnemySingleCombat | EnemyMultiCombat

data CombatInfo = CombatInfo
  { combatTerrain  :: Terrain
  , combatFeature  :: Maybe Feature
  , combatEnemeies :: [ (Enemy, EnemyLifeSpan) ]
  }


-- | Reveal all hidden enemies, remove them from map etc.
startCombatAt :: PlayerName -> Addr -> Land -> Maybe (CombatInfo, Land)
startCombatAt pn a l =
  do ((i,es1),l1) <- updateAddr' a  upd l
     (es2,l2)     <- spawnCombatEnemies pn i l1
     return (CombatInfo { combatTerrain  = hexTerrain i
                        , combatFeature  = hexFeature i
                        , combatEnemeies = es1 ++ es2
                        } , l2)
  where
  upd i = let c  = hexReveal (hexContent i)
              es = zip (hexActiveEnemies c) (repeat EnemyMultiCombat)
              in ((i,es), c)



spawnCombatEnemies :: PlayerName -> HexInfo -> Land ->
                                      Maybe ([(Enemy,EnemyLifeSpan)],Land)
spawnCombatEnemies pn HexInfo { .. } l =
  case hexTerrain of
    City _ -> none
    _ -> case hexFeature of
           Nothing -> none
           Just f ->
             case f of

               MagicalGlade        -> none
               Mine _              -> none
               Village             -> none

               Monastery
                 | isOwned         -> none
                 | otherwise       -> spawn EnemySingleCombat [Mage]

               Keep | ownedByOther -> spawn EnemySingleCombat [Guardian]
                    | otherwise    -> none

               MageTower           -> none

               -- Dungeons and tombs always bring a new creature,
               -- even when conquered.
               Dungeon             -> spawn EnemySingleCombat [Underworld]
               Tomb                -> spawn EnemySingleCombat [Draconum]

               MonsterDen
                | not (isOwned || hasEnemies) ->
                   spawn EnemyMultiCombat [Underworld]
                | otherwise -> none

               SpawningGrounds
                | not isOwned ->
                  let enemyNum = length (hexActiveEnemies hexContent)
                      new      = 2 - enemyNum
                  in spawn EnemyMultiCombat (replicate new Underworld)
                | otherwise -> none

               AncientRuins
                 | not isOwned && not hasEnemies ->
                    spawn EnemyMultiCombat
                              [ e | Fight e <- hexRuinsObjective hexContent ]
                 | otherwise -> none

               RampagingEnemy _ -> none
  where
  none         = return ([], l)
  owners       = hexOwners hexContent
  isOwned      = not (null owners)
  ownedByOther = not (null (delete pn owners))
  hasEnemies   = hexHasEnemies hexContent

  spawn v ts   = do (es,l1) <- spawnCreatures ts l
                    return (zip es (repeat v), l1)


-- | Spawn enemies of the required types.
spawnCreatures :: [EnemyType] -> Land -> Maybe ([Enemy], Land)
spawnCreatures tys l =
  case tys of
    []     -> return ([], l)
    t : ts -> do (e, l1) <- spawnCreature t l
                 (es,l2) <- spawnCreatures ts l1
                 return (e : es, l2)

-- | Spawn a creature of the given type.
spawnCreature :: EnemyType -> Land -> Maybe (Enemy, Land)
spawnCreature ty Land { .. } =
  do rq      <- Map.lookup ty enemyPool
     (e,rq1) <- rqTake rq
     return (e, Land { enemyPool = Map.insert ty rq1 enemyPool, .. })


-- | Summon a creature to be used by enemies with sommoner powers.
summonCreature :: Land -> Maybe (Enemy, Land)
summonCreature = spawnCreature Underworld

discardEnemy :: Enemy -> Land -> Land
discardEnemy e Land { .. } =
  Land { enemyPool = Map.adjust (rqDiscard e) (enemyType e) enemyPool, .. }


-- | Try to explore the given address.
-- If successful, returns an updated land, and the number of
-- monasteries that were revealed, so that the units offer can be updated.
-- Fails if the address is explored, or there is no suitable land to put there.
exploreAt :: Addr -> TileAddr -> Land -> Perhaps (Land, Int)
exploreAt loc newTilePos l =
  do (l1,m) <- initialTile False newTilePos l
     return (revealHiddenNeighbours loc l1, m)

-- | Get the feature of a tile at the given address.
getFeatureAt :: Addr -> Land -> Maybe (Terrain, Maybe Feature)
getFeatureAt Addr { .. } Land { .. } =
  do gt <- Map.lookup addrGlobal theMap
     let HexInfo { .. } = gameTileInfo addrLocal gt
     return (hexTerrain, hexFeature)

-- | Get the revealed enemies at the given locaiton.
-- The enemmies also remain on the map.
getRevealedEnemiesAt :: Addr -> Land -> [Enemy]
getRevealedEnemiesAt Addr { .. } Land { .. } =
  fromMaybe [] $
  do gt <- Map.lookup addrGlobal theMap
     let HexInfo { .. } = gameTileInfo addrLocal gt
     return (hexActiveEnemies hexContent)



-- | Setup a new tile at the given position.
initialTile :: Bool -> TileAddr -> Land -> Perhaps (Land, Int)
initialTile noCheck addr l =
  do (t,l1) <- selectTile noCheck addr l
     let (gt,ms,l2) = populateTile t l1
     return (l2 { theMap = Map.insert addr gt (theMap l2) }, ms)

-- | Update a location on the map.
updateAddr :: Addr -> (HexInfo -> HexContent) -> Land -> Land
updateAddr Addr { .. } f Land { .. } =
  Land { theMap = Map.adjust (gameTileUpdateAt addrLocal f) addrGlobal theMap
       , .. }

-- | Update a location on the map, returning a result.
updateAddr' :: Addr -> (HexInfo -> (a, HexContent)) -> Land -> Maybe (a, Land)
updateAddr' Addr { .. } f Land { .. } =
  do gt <- Map.lookup addrGlobal theMap
     let (res, gt1) = gameTileUpdateAt' addrLocal f gt
     return (res, Land { theMap = Map.insert addrGlobal gt1 theMap, .. })


-- | Place a player on the map.  If during the day, reveal *adjecent*
-- relevant locations.  It does not reveal enemies *on* the location.
placePlayer :: Player -> Land -> Land
placePlayer p = revealHiddenNeighbours loc
              . revealHidden loc
              . updateAddr loc (hexAddPlayer p . hexContent)
  where loc = playerLocation p

-- | Remove a player from the map.
removePlayer :: Player -> Land -> Land
removePlayer p = updateAddr (playerLocation p) (hexRemovePlayer p . hexContent)

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




-- | Compute which addresses start wars, if we move from one location
-- to another normally (i.e., "walking").
provoked :: Land -> Addr -> Addr -> [(Addr,[Enemy])]
provoked Land { .. } from to =
  maybeToList (hasWar isDangerous to) ++
  ( mapMaybe (hasWar isRampaging)
  $ Set.toList
  $ Set.intersection (neighboursOf from) (neighboursOf to))
  where
  neighboursOf x = Set.fromList [ neighbour x d | d <- allDirections ]

  hasWar p a@Addr { .. } =
    do gt <- Map.lookup addrGlobal theMap
       let i = gameTileInfo addrLocal gt
       guard (p i)
       let es = hexActiveEnemies (hexContent i)
       guard (not (null es))
       return (a,es)

  isRampaging HexInfo { hexFeature = Just (RampagingEnemy _) } = True
  isRampaging _ = False

  -- XXX: Not dealing with other players

  isDangerous HexInfo { .. } =
    case hexTerrain of
      City _ -> True
      _ -> case hexFeature of
             Just Keep      -> True
             Just MageTower -> True
             _              -> False


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


searchLand :: (HexInfo -> Bool) -> Land -> [Addr]
searchLand p Land { .. } =
  [ Addr { .. } | (addrGlobal, t) <- Map.toList theMap
                , addrLocal       <- gameTileSearch p t
  ]

-- | How many keeps are owned by this player.
numberOfOwnedKeeps :: PlayerName -> Land -> Int
numberOfOwnedKeeps p = length . searchLand hasKeep
  where
  hasKeep h =
    case hexFeature h of
      Just Keep -> hexHasShield p (hexContent h)
      _         -> False


-- | How many extra cards does a player get based on their location
-- (i.e., due to conquered keeps and cities)
locationCardBonus :: Player -> Land -> Int
locationCardBonus p l = maximum $ map bounus
                                $ loc : [ neighbour loc d | d <- allDirections ]
  where
  loc = playerLocation p

  bounus Addr { .. } =
    fromMaybe 0 $
    do gt <- Map.lookup addrGlobal (theMap l)
       let HexInfo { .. } = gameTileInfo addrLocal gt
           pn             = playerName p
       case hexTerrain of
         City _ -> case hexOwners hexContent of
                     q : qs | pn == q       -> Just 2
                            | pn `elem` qs  -> Just 1
                     _                      -> Nothing

         _ -> case hexFeature of
                Just Keep
                  | hexHasShield pn hexContent -> Just (numberOfOwnedKeeps pn l)
                _ -> Nothing



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
                  Just (gameTilePlaceHolder tileType, valid tileType False)
               (_, Tile { .. } : _) ->
                  Just (gameTilePlaceHolder tileType, valid tileType True)

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



