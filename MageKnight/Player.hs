{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module MageKnight.Player
  ( -- * Basics

    Player
  , PlayerName
  , playerName
  , newPlayer

  -- * Deeds
  , drawCard
  , takeCard
  , newDeed
  , newDiscardedDeed

  -- * Reputation
  , playerReputation
  , playerSetReputation

  -- * Fame
  , playerFame
  , playerLevel
  , playerArmor
  , playerCardLimit
  , playerAddFame

  -- * Crystals
  , addCrystal
  , removeCrystal

  -- * Units
  , hireUnit
  , disbandUnit
  , addUnitSlot
  , woundUnit
  , healUnit
  , unitSetReady
  , unitToggleReady

  -- * Damage
  , assignDamage

  -- * Position
  , playerLocation
  , playerSetLoc
  , backToSafety
  ) where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Bag
import MageKnight.Terrain
import MageKnight.JSON
import MageKnight.Random(StdGen, shuffle)

import           Data.Maybe (isNothing)
import           Data.Text (Text)
import           Data.List (partition)
import           Control.Monad(guard)

type PlayerName = Text

data Player = Player
  { name        :: PlayerName
  , fame        :: Int
  , reputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , units       :: [ Maybe ActiveUnit ]
  , crystals    :: Bag BasicMana
  , deedDeck    :: [ Deed ]
  , hand        :: [ Deed ]
  , discardPile :: [ Deed ]
  , location    :: Addr

  , onUnsafe    :: Maybe (Addr, Int)
    -- ^ `Nothing`, if we are currently safe.
    -- If unsafe, then last safe location, and how many wounds to get there.

  , rng         :: StdGen
    -- ^ To shuffle the deed deck.
  }

instance Eq Player where
  x == y = name x == name y

instance Ord Player where
  compare x y = compare (name x) (name y)


-- | Player's name
playerName :: Player -> PlayerName
playerName = name

-- | Player's reputation
playerReputation :: Player -> Int
playerReputation = reputation

-- | Change the player's reputation
playerSetReputation :: Int -> Player -> Maybe Player
playerSetReputation n Player { .. } =
  do guard (n >= -7 && n <= 7)
     return Player { reputation = n, .. }

-- | Location of the player
playerLocation :: Player -> Addr
playerLocation = location



-- | A new player with the given deck.
newPlayer :: StdGen -> Text -> [Deed] -> Player
newPlayer g name deeds = Player
  { name        = name
  , fame        = 0
  , reputation  = 0
  , units       = [ Nothing ]
  , crystals    = bagEmpty
  , deedDeck    = shuffledDeeds
  , hand        = []
  , discardPile = []
  , location    = Addr { addrGlobal = (0,0), addrLocal = Center }
  , onUnsafe    = Nothing
  , rng         = g1
  }
  where (shuffledDeeds, g1) = shuffle g deeds


-- | Move a card from the player's deed deck to their hand.
drawCard :: Player -> Maybe Player
drawCard Player { .. } =
  case deedDeck of
    []     -> Nothing
    x : xs -> Just Player { hand = x : hand, deedDeck = xs, .. }

-- | Add a card to the player's deed deck.
newDeed :: Deed -> Player -> Player
newDeed d Player { .. } = Player { deedDeck = d : deedDeck, .. }

-- | Add a card to the player's discard pile.
newDiscardedDeed :: Deed -> Player -> Player
newDiscardedDeed d Player { .. } = Player { discardPile = d : discardPile, .. }

-- | Remove a card from the player's hand.
takeCard :: Int -> Player -> Maybe (Deed, Player)
takeCard n Player { .. } =
  case splitAt n hand of
    (as,b:bs) -> Just (b, Player { hand = as ++ bs, .. })
    _         -> Nothing



-- | Add a crystal to the player's inventory.
-- Does nothign if the inventory already has 3 crystals of this color.
addCrystal :: BasicMana -> Player -> Maybe Player
addCrystal c Player { .. } =
  do guard (bagLookup c crystals < 3)
     return Player { crystals = bagAdd 1 c crystals, .. }

-- | Use up one of the player's crystals.
removeCrystal :: BasicMana -> Player -> Maybe Player
removeCrystal c Player { .. } =
  do cs <- bagRemove 1 c crystals
     return Player { crystals = cs, .. }




-- | Player's fame
playerFame :: Player -> Int
playerFame = fame


-- | Change the player's fame by this much.
-- Negative numbers decrease fame.
playerAddFame :: Int -> Player -> Maybe Player
playerAddFame n Player { .. } =
  do guard (val >= 0 && val <= 119)
     return Player { fame = val, .. }
  where val   = fame + n

-- | Amount of fame for each level on the fame board.
levels :: [Int]
levels = levelStart
  where
  levelLens  = 3 : map (+2) levelLens
  levelStart = 0 : zipWith (+) levelStart levelLens

-- | What's the player's level, based on their fame.
playerLevel' :: Player -> (Int,(Int,Int))
playerLevel' Player { .. } = head (dropWhile skip lvls)
  where
  lvls           = zip [1 .. ] (zip levels (map (subtract 1) (tail levels)))
  skip (_,(_,b)) = b < fame

-- | What level is this player
playerLevel :: Player -> Int
playerLevel = fst . playerLevel'

-- | What's the player's default armour, based on their fame.
playerArmor :: Player -> Int
playerArmor p
  | l <= 2    = 2
  | l <= 6    = 3
  | otherwise = 4
    where l = playerLevel p

-- | What's the player's default card limit, based on their fame.
playerCardLimit :: Player -> Int
playerCardLimit p
  | l <= 4    = 5
  | l <= 8    = 6
  | otherwise = 7
    where l = playerLevel p



-- | Check if this player passed out.
-- The blloean indicates if the player passed out.
checkPassOut :: Player -> (Bool, Player)
checkPassOut p@Player { .. }
  | length wounds >= playerCardLimit p =
      (True, Player { hand = wounds, discardPile = others ++ discardPile, .. })
  | otherwise = (False, Player { .. })
  where
  (wounds,others) = partition (== wound) hand

-- | Assign combat damage to a player.  Returns 'True' if the player passed out.
assignDamage :: Int -> Bool {- ^ Poison? -} -> Player -> (Bool, Player)
assignDamage d poison p =
  checkPassOut p { hand = replicate woundNum wound ++ hand p
                 , discardPile = replicate poisonDamage wound ++ discardPile p
                 }
  where
  armor         = playerArmor p
  woundNum      = div (max 0 d + armor - 1) armor
  poisonDamage  = if poison then woundNum else 0

-- | Set the player's location
playerSetLoc :: Bool {- ^ Is this a safe locaiton? -} ->
               Addr -> Player -> Player
playerSetLoc safe loc Player { .. } =
  Player { location = loc
         , onUnsafe =
             if safe then Nothing
                     else Just $ case onUnsafe of
                                   Nothing      -> (location, 0)
                                   Just (sl,ws) -> let ws' = ws + 1
                                                    in seq ws' (sl,ws')
         , .. }



-- | Move the player back to their last safe location.
-- Return 'True' if the player passed out in the process.
backToSafety :: Player -> (Bool, Player)
backToSafety Player { .. } =
  case onUnsafe of
    Nothing    -> (False, Player { .. })
    Just (a,w) ->
      checkPassOut Player { location = a, hand = replicate w wound ++ hand, .. }

-- | Try to add a unit to the player.  Failes if there are no open slots.
-- XXX: Active unite
hireUnit :: Unit -> Player -> Maybe Player
hireUnit u Player { .. } =
  case break isNothing units of
    (as,_:bs) -> Just Player { units = as ++ Just (activeateUnit u) : bs, .. }
    _         -> Nothing

-- | Disband the given unit
disbandUnit :: Int -> Player -> Maybe (Unit, Player)
disbandUnit u Player { .. } =
  case splitAt u units of
    (as,Just b:bs) -> Just (baseUnit b, Player { units = as ++ Nothing : bs
                                               , .. })
    _              -> Nothing


updateUnit :: (ActiveUnit -> ActiveUnit) -> Int -> Player -> Maybe Player
updateUnit f u Player { .. } =
  case splitAt u units of
    (as, Just b : bs) -> Just Player { units = as ++ Just (f b) : bs, .. }
    _                 -> Nothing

woundUnit :: Int -> Player -> Maybe Player
woundUnit = updateUnit $ \ActiveUnit { .. } ->
                          ActiveUnit { unitWounds = 1 + unitWounds, .. }

healUnit :: Int -> Player -> Maybe Player
healUnit = updateUnit $ \ActiveUnit { .. } ->
                          if unitWounds > 0
                            then ActiveUnit { unitWounds = unitWounds - 1, .. }
                            else ActiveUnit { .. }


unitSetReady :: Bool -> Int -> Player -> Maybe Player
unitSetReady r = updateUnit $ \ActiveUnit { .. } ->
                               ActiveUnit { unitReady = r, .. }

unitToggleReady :: Int -> Player -> Maybe Player
unitToggleReady = updateUnit $ \ActiveUnit { .. } ->
                                ActiveUnit { unitReady = not unitReady, .. }




-- | Add an additional slot for a unit
addUnitSlot :: Player -> Player
addUnitSlot Player { .. } = Player { units = units ++ [Nothing], .. }

instance Export Player where
  toJS Player { .. } =
    object
      [ "name"        .= name
      , "fameInfo"    .= object [ "fame"  .= fame
                                , "level" .= l
                                , "start" .= start
                                , "end"   .= end
                                ]
      , "reputation"  .= reputation
      , "units"       .= units
      , "crystals"    .= object [ toKeyJS x .= n
                                          | (x,n) <- bagToListGrouped crystals ]
      , "cards"       .= hand
      , "location"    .= location
      , "unsafe"      .= fmap fst onUnsafe
      ]
    where (l,(start,end)) = playerLevel' Player { .. }

