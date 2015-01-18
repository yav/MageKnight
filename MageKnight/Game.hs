{-# LANGUAGE RecordWildCards #-}
module MageKnight.Game where

import           MageKnight.Common
import           MageKnight.Bag
import           MageKnight.Units
import           MageKnight.Terrain
import           MageKnight.GameTile
import           MageKnight.Enemies
import           MageKnight.Ruins
import           MageKnight.Cards
import           MageKnight.HexContent
import           MageKnight.ResourceQ ( ResourceQ )
import qualified MageKnight.ResourceQ as RQ

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad(guard)
import           System.Random (StdGen, split)



data Offer a = Offer
  { offerDeck :: ResourceQ a
  , offering  :: [a]  -- newest first
  }

offerEmpty :: StdGen -> [a] -> Offer a
offerEmpty r is = Offer { offerDeck = RQ.fromListRandom r is, offering  = [] }

offerDrawItem :: Offer a -> Offer a
offerDrawItem Offer { .. } =
  case RQ.take offerDeck of
    Just (c,d) -> Offer { offerDeck = d, offering = c : offering }
    Nothing    -> Offer { .. }

offerDrawItems :: Int -> Offer a -> Offer a
offerDrawItems n o = iterate offerDrawItem o !! n

offerDiscardAll :: Offer a -> Offer a
offerDiscardAll Offer { .. } = Offer
  { offering = []
  , offerDeck = foldr RQ.discard offerDeck offering
  }

offerTakeItem :: Int -> Offer a -> Maybe (a, Offer a)
offerTakeItem n0 Offer { .. }
  | n0 >= 0    = do (c,os) <- go n0 offering
                    return (c, Offer { offering = os, .. })
  | otherwise = Nothing
  where
  go _ []       = Nothing
  go 0 (c : cs) = Just (c, cs)
  go n (c : cs) = do (a,as) <- go (n-1) cs
                     return (a, c : as)

offerReturnItem :: a -> Offer a -> Offer a
offerReturnItem a Offer { .. } =
  Offer { offerDeck = RQ.discard a offerDeck, .. }

offeringLength :: Offer a -> Int
offeringLength Offer { .. } = length offering


--------------------------------------------------------------------------------

newCardOffer :: StdGen -> [Card] -> Offer Card
newCardOffer r cs = offerDrawItems 3 (offerEmpty r cs)

refreshCardOffer :: Offer Card -> Maybe (Offer Card)
refreshCardOffer Offer { .. } =
  case reverse offering of
    [] -> Nothing
    x : xs -> Just $ offerDrawItem
                   $ offerReturnItem x
                     Offer { offering  = reverse xs, .. }

refreshCardOfferDummy :: Offer Card -> Maybe (Card, Offer Card)
refreshCardOfferDummy Offer { .. } =
  case reverse offering of
    [] -> Nothing
    x : xs -> Just (x, offerDrawItem Offer { offering = reverse xs, .. })

offerTakeCard :: Int -> Offer Card -> Maybe (Card, Offer Card)
offerTakeCard n o =
  do (c,o1) <- offerTakeItem n o
     return (c, offerDrawItem o1)

offerReturnCards :: [Card] -> Offer Card -> Offer Card
offerReturnCards cs o = foldr offerReturnItem o cs

--------------------------------------------------------------------------------

data UnitOffer = UnitOffer
  { regularUnits :: Offer Unit
  , eliteUnits   :: Offer Unit
  }

unitsOnOffer :: UnitOffer -> [Unit]
unitsOnOffer UnitOffer { .. } = offering regularUnits ++ offering eliteUnits

emptyUnitOffer :: StdGen -> [Unit] -> [Unit] -> UnitOffer
emptyUnitOffer g regular elite = UnitOffer
  { regularUnits = offerEmpty g1 regular
  , eliteUnits   = offerEmpty g2 elite
  }
  where
  (g1,g2) = split g

refreshOnlyRegulars :: Int -> UnitOffer -> UnitOffer
refreshOnlyRegulars playerNum UnitOffer { .. } = UnitOffer
  { regularUnits = offerDrawItems (playerNum + 2) (offerDiscardAll regularUnits)
  , eliteUnits   = offerDiscardAll eliteUnits
  }

refreshWithElite :: Int -> UnitOffer -> UnitOffer
refreshWithElite playerNum UnitOffer { .. } = UnitOffer
  { regularUnits = offerDrawItems regularNum (offerDiscardAll regularUnits)
  , eliteUnits   = offerDrawItems eliteNum   (offerDiscardAll eliteUnits)
  }
  where
  totalNum   = playerNum + 2
  regularNum = div totalNum 2
  eliteNum   = totalNum - regularNum

offerTakeUnit :: Int -> UnitOffer -> Maybe (Unit, UnitOffer)
offerTakeUnit n UnitOffer { .. }
  | n < 0 = Nothing
  | Just (u,rs) <- offerTakeItem n regularUnits =
                        Just (u, UnitOffer { regularUnits = rs, .. })
  | otherwise =
    do (u,es) <- offerTakeItem (n - offeringLength regularUnits) eliteUnits
       return (u, UnitOffer { eliteUnits = es, .. })

offerDisbandUnit :: Unit -> UnitOffer -> UnitOffer
offerDisbandUnit u UnitOffer { .. } =
  case unitType u of
    RegularUnit -> UnitOffer { regularUnits = offerReturnItem u regularUnits,..}
    EliteUnit -> UnitOffer { eliteUnits = offerReturnItem u eliteUnits, .. }

--------------------------------------------------------------------------------



data Offers = Offers
  { advancedActionOffer :: Offer Card
  , spellOffer          :: Offer Card
  , unitOffer           :: UnitOffer
  , monasteryTech       :: [Card]
  , artifactDeck        :: ResourceQ Card
  }

newMonastery :: Offers -> Offers
newMonastery Offers { .. } =
  case RQ.take (offerDeck advancedActionOffer) of
    Nothing -> Offers { .. }
    Just (c,d) -> Offers
      { advancedActionOffer = advancedActionOffer { offerDeck = d }
      , monasteryTech = c : monasteryTech
      , ..
      }

data Game = Game
  { gameTime        :: Time
  , theSource       :: Bag Mana
  , offers          :: Offers
  , enemyPool       :: Map EnemyType (ResourceQ Enemy)
  , ruinsPool       :: ResourceQ Ruins

  , theMap          :: Map TileAddr GameTile
  , mapShape        :: MapShape
  , unexploredTiles :: [ Tile ]
  , backupTiles     :: [ Tile ] -- to be used if we run out of the scenario ones
                                -- base should be before advanced
  , cityLevels      :: [Int]
  , unexploredRuins :: ResourceQ Ruins
  }


blankEmenyPool :: StdGen -> Map EnemyType (ResourceQ Enemy)
blankEmenyPool g0 = snd $ foldr add (g0,Map.empty) allEnemyTypes
  where
  add e (g,m) = let (g1,g2) = split g
                in (g2, Map.insert e (RQ.empty g1) m)

initialEnemyPool :: StdGen -> Map EnemyType (ResourceQ Enemy)
initialEnemyPool g0 = foldr add (blankEmenyPool g0) allEnemies
  where
  add e qs = Map.adjust (RQ.discard e) (enemyType e) qs


-- | Check if there are any more tiles available.  If so, also check
-- if the next tile may be placed at the given location.
selectTile :: TileAddr -> Game -> Maybe (Tile, Game)
selectTile pt Game { .. }
  | t : ts <- unexploredTiles =
    do guard (validPlacement mapShape explored (tileType t) False pt)
       return (t, Game { unexploredTiles = ts, .. })

  | t : ts <- backupTiles =
    do guard (validPlacement mapShape explored (tileType t) True pt)
       return (t, Game { backupTiles = ts, .. })

  | otherwise = Nothing
  where
  explored a = a `Map.member` theMap


populateTile :: Tile -> Game -> (GameTile, Game)
populateTile gameTile g = (GameTile { .. }, g1)
  where
  (gameTileContent, g1) = foldr setupHex (Map.empty,g) allHexAddrs

  setupHex a (c,Game { .. }) =
    let nothing   = (c, Game { .. })
        addHex h  = Map.insert a h c
        enemy v t = case hexWithEnemy v t enemyPool of
                      (h,p) -> (addHex h, Game { enemyPool = p, .. })

    in case tileTerrain gameTile a of

         (City color, _) | l : ls <- cityLevels ->
           case hexWithCity color l enemyPool of
             (h, p) -> (addHex h, Game { enemyPool = p, cityLevels = ls, .. })

         (_, Just feature) ->
           case feature of
             MagicalGlade -> nothing
             Mine _     -> nothing
             Village    -> nothing
             Monastery  -> (c, Game { offers = newMonastery offers, .. })
             Keep       -> enemy Hidden Guardian
             MageTower  -> enemy Hidden Mage
             Dungeon    -> nothing
             Tomb       -> nothing
             MonsterDen -> nothing
             SpawningGrounds   -> nothing
             AncientRuins      ->
               case hexWithRuins gameTime ruinsPool of
                 (h,r) -> (addHex h, Game { ruinsPool = r, .. })
             RampagingEnemy et -> enemy Revealed et

         _ -> nothing


-- | Try to explore from the given position, in the given direction.
exploreInDir :: Addr -> Dir -> Game -> Maybe Game
exploreInDir addr dir g =
  do let newTilePos = addrGlobal (neighbour addr dir)
     (t,g1) <- selectTile newTilePos g
     let (gt,g2) = populateTile t g1
         g3 = g2 { theMap = Map.insert newTilePos gt (theMap g2) }
     return (revealHiddenNeighbours addr g3)

revealHiddenNeighbours :: Addr -> Game -> Game
revealHiddenNeighbours a Game { .. } =
  Game { theMap = foldr checkAt theMap (map (neighbour a) allDirections), .. }
  where
  checkAt Addr { .. } mp = Map.adjust (gameTileUpdateAt addrLocal check)
                                      addrGlobal mp

  check t f h = if shouldReveal t f then hexReveal h else h

  shouldReveal (City _) _         = True
  shouldReveal _ (Just Keep)      = gameTime == Day
  shouldReveal _ (Just MageTower) = gameTime == Day
  shouldReveal _ _                = False


