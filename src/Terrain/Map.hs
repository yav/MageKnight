module Terrain.Map
  ( -- * Setting up the land
    Land
  , LandSetup(..), defaultLandSetup, setupLand

    -- * Exploration
  , setPlayer
  , getPlayerNeighbours
  , exploreAt
  , isRevealed

  , startsBattle

    -- * Time
  , setTime
  , getTime

    -- * Info about tiles
  , getFeatureAt
  , getRevealedEnemiesAt
--  , locationCardBonus

    -- * Combat
  , EnemyLifeSpan(..)
  -- , CombatInfo(..)
  -- , startCombatAt
  , summonCreature
  , discardEnemy

  ) where

import GHC.Generics
import           Data.Maybe ( mapMaybe, fromMaybe, maybeToList )
import           Data.Map ( Map )
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import           Control.Monad( foldM, guard )

import Data.Aeson(ToJSON,toJSON)

import KOI.Utils
import KOI.RNGM

import  Util.ResourceQ
import  Util.Perhaps



import  Terrain.Type
import  Terrain.Hex
import  Terrain.Tile

import  Enemies( Enemy(..), EnemyType(..), allEnemies, allEnemyTypes )
import  Ruins(Ruins, ruins)
import  Common(Time(..), Visibility(..))


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
                      , playerLocation  = Addr (head startPositions) Center
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

  , playerLocation  :: Addr
    -- ^ Where is the player.
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
-- This is returned so that we can add the appropriate monasetry tech to
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

         HexLandInfo { hexFeature = Just feature } ->
           case feature of
             MagicalGlade      -> nothing
             Mine _            -> nothing
             Village           -> nothing
             Monastery         -> (c, 1 + ms, Land { .. })
             Keep              -> enemy Hidden Guardian
             MageTower         -> enemy Hidden Mage
             City color ->
               case cityLevels of
                 l : ls ->
                   case hexWithCity color l enemyPool of
                     (h, p) -> (addHex h, ms,
                                Land { enemyPool = p, cityLevels = ls, .. })
                 [] -> nothing

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

  check HexInfo { hexLandInfo = HexLandInfo { .. }, .. }
    | shouldReveal hexTerrain hexFeature = hexReveal hexContent
    | otherwise                          = hexContent

  shouldReveal _ (Just (City _))  = True
  shouldReveal _ (Just Keep)      = timeOfDay == Day
  shouldReveal _ (Just MageTower) = timeOfDay == Day
  shouldReveal _ _                = False


-- | Reveal information when a player enters a hex.
revealHidden :: Addr -> Land -> Land
revealHidden a l = updateAddr a (\h -> upd h (hexContent h)) l
  where
  upd HexInfo { hexLandInfo = HexLandInfo { .. } } =
    case hexTerrain of
      _ -> case hexFeature of
             Just AncientRuins -> hexReveal
             Just MageTower    -> hexReveal
             Just Keep         -> hexReveal
             Just (City _)     -> hexReveal
             _                 -> id


data EnemyLifeSpan = EnemySummoned | EnemySingleCombat | EnemyMultiCombat

data CombatInfo = CombatInfo
  { combatTerrain  :: HexLandInfo
  , combatEnemeies :: [ (Enemy, EnemyLifeSpan) ]
  }

-- | Spawn enemies of the required types.
spawnCreatures :: [EnemyType] -> Land -> Perhaps ([Enemy], Land)
spawnCreatures tys l =
  case tys of
    []     -> return ([], l)
    t : ts -> do (e, l1) <- spawnCreature t l
                 (es,l2) <- spawnCreatures ts l1
                 return (e : es, l2)

-- | Spawn a creature of the given type.
-- Fails if there are no more enemies available of the required type.
spawnCreature :: EnemyType -> Land -> Perhaps (Enemy, Land)
spawnCreature ty Land { .. } =
  perhaps (Text.unwords [ "Insufficient", showText ty, "enemies."]) $
  do rq      <- Map.lookup ty enemyPool
     (e,rq1) <- rqTake rq
     return (e, Land { enemyPool = Map.insert ty rq1 enemyPool, .. })


-- | Summon a creature to be used by enemies with sommoner powers.
summonCreature :: Land -> Perhaps (Enemy, Land)
summonCreature = spawnCreature Underworld

discardEnemy :: Enemy -> Land -> Land
discardEnemy e Land { .. } =
  Land { enemyPool = Map.adjust (rqDiscard e) (enemyType e) enemyPool, .. }


-- | Try to explore the given address.
-- If successful, returns an updated land, and the number of
-- monasteries that were revealed, so that the units offer can be updated.
-- Fails if the address is explored, or there is no suitable land to put there.
exploreAt :: TileAddr -> Land -> Perhaps (Land, Int)
exploreAt newTilePos l =
  do let loc = playerLocation l
     (l1,m) <- initialTile False newTilePos l
     return (revealHiddenNeighbours loc l1, m)


-- | Get the features and the conetnt of a tile.
getHexInfo :: Addr -> Land -> Maybe HexInfo
getHexInfo Addr { .. } Land { .. } =
  do gt <- Map.lookup addrGlobal theMap
     return (gameTileInfo addrLocal gt)

-- | Get the feature of a tile at the given address.
getFeatureAt :: Addr -> Land -> Maybe HexLandInfo
getFeatureAt a l = hexLandInfo <$> getHexInfo a l

-- | Get the revealed enemies at the given locaiton.
-- The enemmies also remain on the map.
getRevealedEnemiesAt :: Addr -> Land -> [Enemy]
getRevealedEnemiesAt Addr { .. } Land { .. } =
  fromMaybe [] $
  do gt <- Map.lookup addrGlobal theMap
     let HexInfo { .. } = gameTileInfo addrLocal gt
     return (hexActiveEnemies hexContent)

setPlayer :: Addr -> Land -> Land
setPlayer a l
  | addrGlobal a `Map.member` theMap l =
    (revealHidden a (revealHiddenNeighbours a l)) { playerLocation = a }
  | otherwise = l

getPlayerNeighbours :: Land -> [(Addr, Maybe TileType)]
getPlayerNeighbours l =
  [ (a, ex) | a <- candidates
  , let g = addrGlobal a
  , ex <- if known g then [Nothing]
                     else case canExplore g of
                            Nothing -> []
                            it      -> [it]
  ]
  where
  loc        = playerLocation l
  candidates = Set.toList (neighboursUpTo 1 loc)

  explo = case (unexploredTiles l, backupTiles l) of
            ([],[])    -> Nothing
            (t : _, _) -> Just (tileType t, False)
            (_, t : _) -> Just (tileType t, True)

  known g = g `Map.member` theMap l

  valid = validPlacement (mapShape l) known

  canExplore g =
    do (ty,isBak) <- explo
       guard (valid ty isBak g)
       pure ty



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


-- | Compute which addresses start wars, if we move from one location
-- to another normally (i.e., "walking").
startsBattle :: Land -> Addr -> Addr -> [(Addr,[Enemy])]
startsBattle Land { .. } fromS toS =
  maybeToList (hasWar isDangerous toS) ++
  mapMaybe (hasWar isRampaging)
    (Set.toList (Set.intersection (neighboursOf fromS) (neighboursOf toS)))
  where
  neighboursOf x = Set.fromList [ neighbour x d | d <- allDirections ]

  hasWar p a@Addr { .. } =
    do gt <- Map.lookup addrGlobal theMap
       let i = gameTileInfo addrLocal gt
       guard (p i)
       let es = hexActiveEnemies (hexContent i)
       guard (not (null es))
       return (a,es)

  isRampaging HexInfo
               { hexLandInfo =
                   HexLandInfo { hexFeature = Just (RampagingEnemy _) }
               } = True
  isRampaging _ = False

  isDangerous HexInfo { hexLandInfo = HexLandInfo { .. } } =
      case hexFeature of
        Just (City _)  -> True
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

-- | Is this a safe location for the player.
isSafe :: Addr -> Land -> Bool
isSafe Addr { .. } Land { .. } =
  case Map.lookup addrGlobal theMap of
    Nothing -> False
    Just gt -> gameTileIsSafe gt addrLocal


searchLand :: (HexInfo -> Bool) -> Land -> [Addr]
searchLand p Land { .. } =
  [ Addr { .. } | (addrGlobal, t) <- Map.toList theMap
                , addrLocal       <- gameTileSearch p t
  ]


--------------------------------------------------------------------------------

data LandView = LandView
  { landMap     :: Map TileAddr GameTile
  , landTime    :: Time
  , landPlayer  :: Addr
  } deriving (Generic,ToJSON)

mkLandView :: Land -> LandView
mkLandView l = LandView
  { landMap     = theMap l
  , landTime    = timeOfDay l
  , landPlayer  = playerLocation l
  }

instance ToJSON Land where
  toJSON = toJSON . mkLandView

