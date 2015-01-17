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


data Offer = Offer
  { offerDeck :: ResourceQ Card
  , offering  :: [Card]
  }

data Offers = Offers
  { advancedActionOffer :: Offer
  , spellOffer          :: Offer
  , unitOffer           :: Offer
  , monasteryTech       :: [Card]
  , artifactDeck        :: ResourceQ Card
  }

newMonastery :: Offers -> Offers
newMonastery Offers { .. } =
  case RQ.take (offerDeck advancedActionOffer) of
    Nothing    -> Offers { .. }
    Just (c,d) -> Offers { advancedActionOffer = advancedActionOffer
                                                              { offerDeck = d }
                         , monasteryTech = c : monasteryTech
                         , .. }

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
             MagicalGlade      -> nothing
             Mine _            -> nothing
             Village           -> nothing
             Monastery         -> (c, Game { offers = newMonastery offers, .. })
             Keep              -> enemy Hidden Guardian
             MageTower         -> enemy Hidden Mage
             Dungeon           -> nothing
             Tomb              -> nothing
             MonsterDen        -> nothing
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


