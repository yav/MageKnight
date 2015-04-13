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

import           Data.Text (Text)
import           Data.List (partition)

type PlayerName = Text

data Player = Player
  { name        :: PlayerName
  , fame        :: Int
  , reputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , units       :: [ Maybe Unit ]   -- XXX: Replace With active units
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
playerSetReputation :: Int -> Player -> Player
playerSetReputation n Player { .. } = Player { reputation = n', .. }
  where n' = if n >= -7 && n <= 7 then n else reputation

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
drawCard :: Player -> Player
drawCard Player { .. } =
  case deedDeck of
    []     -> Player { .. }
    x : xs -> Player { hand = x : hand, deedDeck = xs, .. }

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
addCrystal :: BasicMana -> Player -> Player
addCrystal c Player { .. }
  | bagLookup c crystals >= 3 = Player { .. }
  | otherwise = Player { crystals = bagAdd 1 c crystals, .. }

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
playerAddFame :: Int -> Player -> Player
playerAddFame n Player { .. } = Player { fame = fame', .. }
  where val   = fame + n
        fame' = if val >= 0 && val <= 119 then val else fame


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
  | l <= 4  = 5
  | l <= 8  = 6
  | otherwise         = 7
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
assignDamage :: Int -> Player -> (Bool, Player)
assignDamage d p = checkPassOut p { hand = replicate woundNum wound ++ hand p }
  where
  armor       = playerArmor p
  woundNum    = div (max 0 d + armor - 1) armor


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
      , "crystals"    .= bagToList crystals
      , "cards"       .= hand
      , "location"    .= location
      , "unsafe"      .= fmap fst onUnsafe
      ]
    where (l,(start,end)) = playerLevel' Player { .. }

