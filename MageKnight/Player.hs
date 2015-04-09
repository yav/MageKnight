{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module MageKnight.Player where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Bag
import MageKnight.Terrain
import MageKnight.JSON

import           Data.Text (Text)

type PlayerName = Text

data Player = Player
  { playerName        :: PlayerName
  , playerFame        :: Int
  , playerReputation  :: Int      -- ^ Index on board, *NOT* same as influence
  , playerUnits       :: [ Maybe Unit ]   -- XXX: Replace With active units
  , playerCrystals    :: Bag BasicMana
  , playerDeedDeck    :: [ Deed ]
  , playerHand        :: Bag Deed
  , playerDiscardPile :: [ Deed ]
  , playerLocation    :: Addr

  , playerOnUnsafe    :: Maybe (Addr, Int)
    -- ^ `Nothing`, if we are currently safe.
    -- If unsafe, then last safe location, and how many wounds to get there.
  }


newPlayer :: Text -> [Deed] -> Player
newPlayer name deeds = Player
  { playerName        = name
  , playerFame        = 0
  , playerReputation  = 0
  , playerUnits       = [ Nothing ]
  , playerCrystals    = bagEmpty
  , playerDeedDeck    = deeds
  , playerHand        = bagEmpty
  , playerDiscardPile = []
  , playerLocation    = Addr { addrGlobal = (0,0), addrLocal = Center }

  , playerOnUnsafe    = Nothing
  }

levels :: [Int]
levels = levelStart
  where
  levelLens  = 3 : map (+2) levelLens
  levelStart = 0 : zipWith (+) levelStart levelLens

-- | What's the player's level, based on their fame.
playerLevel' :: Player -> (Int,(Int,Int))
playerLevel' Player { .. } = head (dropWhile skip lvls)
  where
  lvls = zip [1 .. ] (zip levels (map (subtract 1) (tail levels)))
  skip (_,(_,b)) = b < playerFame

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


instance Eq Player where
  x == y = playerName x == playerName y

instance Ord Player where
  compare x y = compare (playerName x) (playerName y)


-- | Use up one of the player's crystals.
removeCrystal :: BasicMana -> Player -> Maybe Player
removeCrystal c Player { .. } =
  do cs <- bagRemove 1 c playerCrystals
     return Player { playerCrystals = cs, .. }


-- | Check if this player passed out.
-- The blloean indicates if the player passed out.
checkPassOut :: Player -> (Bool, Player)
checkPassOut p@Player { .. }
  | woundNum >= playerCardLimit p =
      ( True, Player
                { playerHand = bagAdd woundNum wound bagEmpty
                , playerDiscardPile = bagToList (bagRemoveAll wound playerHand)
                , ..
                } )
  | otherwise = (False, Player { .. })
  where
  woundNum = bagLookup wound playerHand

-- | Assign combat damage to a player.  Returns 'True' if the player passed out.
assignDamage :: Int -> Player -> (Bool, Player)
assignDamage d p = checkPassOut
                      p { playerHand = bagAdd woundNum wound (playerHand p) }
  where
  armor       = playerArmor p
  woundNum    = div (max 0 d + armor - 1) armor


-- | Move the player back to their last safe location.
-- Return 'True' if the player passed out in the process.
backToSafety :: Player -> (Bool, Player)
backToSafety p =
  case playerOnUnsafe p of
    Nothing    -> (False, p)
    Just (a,w) -> checkPassOut p { playerLocation = a
                                 , playerHand = bagAdd w wound (playerHand p)
                                 }

instance Export Player where
  toJS Player { .. } =
    object
      [ "name"        .= playerName
      , "fameInfo"    .= object [ "fame"  .= playerFame
                                , "level" .= l
                                , "start" .= start
                                , "end"   .= end
                                ]
      , "reputation"  .= playerReputation
      , "units"       .= playerUnits
      , "crystals"    .= bagToList playerCrystals
      , "cards"       .= bagToList playerHand
      , "location"    .= playerLocation
      , "unsafe"      .= fmap fst playerOnUnsafe
      ]
    where (l,(start,end)) = playerLevel' Player { .. }

