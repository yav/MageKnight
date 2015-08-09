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
  , deedsEmpty
  , handEmpty

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
  , WoundLocation(..)
  , healWound
  , knockOut

  -- * Position
  , playerLocation
  , playerSetLoc
  , backToSafety

  -- * Resting
  , slowRecovery
  , standardRest

  -- * Level-up
  , nextSkillSet
  , gainSkill

  -- * Tactics
  , setTactic

  , tacticRethink
  , tacticGreatStart
  , tacticLlongNight
  , tacticMidnightMeditation
  , tacticPreparation
  ) where

import MageKnight.Common
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Bag
import MageKnight.Terrain
import MageKnight.Skill
import MageKnight.Tactic
import MageKnight.Perhaps
import MageKnight.JSON
import MageKnight.Random(StdGen, shuffle)

import           Data.Maybe (isNothing,fromMaybe)
import           Data.Text (Text)
import           Data.List (partition)
import           Control.Monad(guard)

type PlayerName = Text

data Player = Player
  { name            :: PlayerName
  , fame            :: Int
  , reputation      :: Int      -- ^ Index on board, *NOT* same as influence
  , units           :: [ Maybe ActiveUnit ]
  , crystals        :: Bag BasicMana
  , deedDeck        :: [ Deed ]
  , hand            :: [ Deed ]
  , discardPile     :: [ Deed ]
  , skills          :: [ (Skill, Usable) ]  -- ^ Aquired skills
  , potentialSkills :: [ Skill ]            -- ^ Used when leveling up
  , tactic          :: Maybe Tactic
  , location        :: Addr
  , rng             :: StdGen
    -- ^ To shuffle the deed deck.


  , prevLocation    :: Maybe Addr
  , onUnsafe        :: Maybe (Addr, Int)
    -- ^ `Nothing`, if we are currently safe.
    -- If unsafe, then last safe location, and how many wounds to get there.

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
newPlayer :: StdGen -> Text -> [Deed] -> [Skill] -> Player
newPlayer g name deeds ski = Player
  { name            = name
  , fame            = 0
  , reputation      = 0
  , units           = [ Nothing ]
  , crystals        = bagEmpty
  , deedDeck        = shuffledDeeds
  , hand            = []
  , discardPile     = []
  , skills          = []
  , tactic          = Nothing
  , location        = Addr { addrGlobal = (0,0), addrLocal = Center }
  , prevLocation    = Nothing
  , onUnsafe        = Nothing
  , rng             = g2
  , ..
  }
  where (shuffledDeeds, g1)   = shuffle g deeds
        (potentialSkills, g2) = shuffle g1 ski


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


deedsEmpty :: Player -> Bool
deedsEmpty Player { .. } = null deedDeck

handEmpty :: Player -> Bool
handEmpty Player { .. } = null hand


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



-- | If a player gets too many wounds in a combat, then they get knocked out.
knockOut :: Player -> Player
knockOut Player { .. } =
  let (wounds,others) = partition (== wound) hand
  in Player { hand = wounds, discardPile = others ++ discardPile, .. }

-- | Assign combat damage to a player.
assignDamage :: Int    {- ^ Damage to assign -} ->
                Bool   {- ^ Poison? -} ->
                Player {- ^ Player to assign the damage to -} ->
                (Int, Player)
                -- ^ How many wounds we assigned and updated player
assignDamage d poison p =
  ( woundNum
  , p { hand        = replicate woundNum wound ++ hand p
      , discardPile = replicate poisonDamage wound ++ discardPile p
      }
  )
  where
  armor         = playerArmor p
  woundNum      = div (max 0 d + armor - 1) armor
  poisonDamage  = if poison then woundNum else 0


data WoundLocation = WoundInHand | WoundInDiscardPile

-- | Remove a wound from the hand.
healWound :: WoundLocation -> Player -> Maybe Player
healWound which Player { .. } =
  case which of

    WoundInHand ->
      do cs <- removeWound hand
         return Player { hand = cs, .. }

    WoundInDiscardPile ->
      do cs <- removeWound discardPile
         return Player { discardPile = cs, .. }


-- | Remove one wound, if possible.
removeWound :: [Deed] -> Maybe [Deed]
removeWound ds0 =
  case ds0 of
    [] -> Nothing
    d : ds | d == wound -> Just ds
           | otherwise  -> (d :) `fmap` removeWound ds


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
backToSafety :: Player -> Player
backToSafety Player { .. } =
  case onUnsafe of
    Nothing    -> Player { .. }
    Just (a,w) -> Player { location = a, hand = replicate w wound ++ hand, .. }

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


slowRecovery :: Player -> Perhaps Player
slowRecovery Player { .. } =
  case hand of
    [] -> Failed "Requires at least one wound"
    c : cs
      | any (/= wound) hand -> Failed "Requires only wounds in hand"
      | otherwise -> Ok Player { hand = cs, discardPile = c : discardPile, .. }

standardRest :: Int {-^ Discard this (non-wound) card -} ->
                Int {-^ Discards this many wounds -} ->
                Player -> Perhaps Player
standardRest i woundNum Player { .. } =
  case splitAt i hand of
    (as,b:bs)
      | b == wound -> Failed "Discarded card cannot be a wound"
      | otherwise ->
        let (ws,os) = getWounds woundNum (as ++ bs)
        in Ok Player { hand = os, discardPile = b : ws ++ discardPile, .. }

    _ -> Failed "Invalid card to discrad"

  where
  getWounds _ [] = ([],[])
  getWounds n cs | n < 1  = ([], cs)
  getWounds n (c : cs)
    | c /= wound = let (xs,ys) = getWounds n cs
                   in (xs, c:ys)
    | otherwise = let (xs,ys) = getWounds (n-1) cs
                  in (c:xs,ys)

--------------------------------------------------------------------------------

-- | Take 2 skills for level-up options.  In the unlikely case that there
-- are not enough skills left, we return whatever is available.
nextSkillSet :: Player -> ([Skill],Player)
nextSkillSet p = (xs, p { potentialSkills = rest })
  where
  (xs,rest) = splitAt 2 (potentialSkills p)

-- | Add a new skill to the player.
gainSkill :: Skill -> Player -> Player
gainSkill s p = p { skills = (s,Unused) : skills p }


--------------------------------------------------------------------------------

setTactic :: Tactic -> Player -> Player
setTactic t p = p { tactic = Just t }

tacticRethink :: [Int] -> Player -> Perhaps Player
tacticRethink xs0 p0
  | length xs0 > 3 = Failed "Cannot rethink ore than 3 cards"
  | otherwise      = go p0 xs0
  where
  go p [] = let (cs, g) = shuffle (rng p) (discardPile p ++ deedDeck p)
            in Ok p { discardPile = []
                    , deedDeck = cs
                    , rng = g
                    }
  go p (x : xs) = do p1 <- rethink p x
                     go p1 xs
  rethink p i =
    case takeCard i p of
      Nothing -> Failed "No such card"
      Just (c,p1) ->
        case drawCard (newDiscardedDeed c p1) of
          Nothing -> Failed "Insufficient cards in deed deck"
          Just p2 -> Ok p2

tacticGreatStart :: Player -> Player
tacticGreatStart = tryDraw . tryDraw
  where tryDraw p = fromMaybe p (drawCard p)

tacticLlongNight :: Player -> Perhaps Player
tacticLlongNight Player { .. } =
  case deedDeck of
    [] -> let (ds,g)  = shuffle rng discardPile
              (as,bs) = splitAt 3 ds
          in Ok Player { deedDeck = as, discardPile = bs, rng = g, .. }

    _  -> Failed "Long night requires an empty deed deck"

tacticMidnightMeditation :: [Int] -> Player -> Perhaps Player
tacticMidnightMeditation xs = go 0 xs
    where
    go n [] p = let (ds,g) = shuffle (rng p) (deedDeck p)
                    p1 = p { deedDeck = ds, rng = g }
                in Ok (iterate (\q -> fromMaybe q (drawCard q)) p1 !! n)
    go n (a : as) p
      | n >= 5 = Failed "Meditation is limited to 5"
      | otherwise =
        case takeCard a p of
          Just (d,p1) -> go (n+1) as p1 { deedDeck = d : deedDeck p1 }
          Nothing     -> Failed "Invalid card"


tacticPreparation :: Int -> Player -> Maybe Player
tacticPreparation n Player { .. } =
  case splitAt n deedDeck of
    (as,b:bs) ->
      let (ds,g) = shuffle rng (as ++ bs)
      in Just Player { hand = b : hand, deedDeck = ds, rng = g, .. }
    _ -> Nothing

--------------------------------------------------------------------------------

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
      , "deeds"       .= length deedDeck
      , "discards"    .= length discardPile
      , "skills"      .= [ object [ "skill" .= s, "usable" .= (u == Unused) ]
                                                            | (s,u) <- skills ]
      , "location"    .= location
      , "unsafe"      .= fmap fst onUnsafe
      ]
    where (l,(start,end)) = playerLevel' Player { .. }

