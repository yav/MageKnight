{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module MageKnight.Offers
  ( -- * Setup
    Offers
  , OfferSetup (..)
  , setupOffers
  , defaultOfferSetup
  , refreshOffers
  , refreshOffersDummy

    -- * What's on offer
  , offeringAdvancedActions
  , offeringSpells
  , offeringUnits
  , offeringMonasteries

    -- * Take items from offers
  , takeAdvancedAction
  , takeSpell
  , takeUnit
  , takeMonasteryTech
  , takeSkill

    -- * Others
  , newSkill
  , newMonastery
  , burnMonastery
  , disbandUnit
  ) where

import           MageKnight.ResourceQ (ResourceQ)
import qualified MageKnight.ResourceQ as RQ
import           MageKnight.Random
import           MageKnight.Units
import           MageKnight.Skill
import           MageKnight.Perhaps
import           MageKnight.DeedDecks
                   ( Deed,advancedActions, spells, interactiveSpell, artifacts)
import           MageKnight.JSON

import Control.Monad(guard)

data Offer a = Offer
  { offerDeck :: ResourceQ a  -- ^ New offers come from here
  , offering  :: [a]          -- ^ Latest offers at the front
  }

-- | A blank offer that will serve the given items.
offerNothing :: StdGen -> [a] -> Offer a
offerNothing r is = Offer { offerDeck = RQ.fromListRandom r is, offering  = [] }

-- | Draw an item from the deck, and add it to the offerings.
offerNewItem :: Offer a -> Offer a
offerNewItem Offer { .. } =
  case RQ.take offerDeck of
    Just (c,d) -> Offer { offerDeck = d, offering = c : offering }
    Nothing    -> Offer { .. }

-- | Draw multiple items to add to the offer.
offerNewItems :: Int -> Offer a -> Offer a
offerNewItems n o = iterate offerNewItem o !! n

-- | Remove everything that's on offer, and return it to the deck.
offerClear :: Offer a -> Offer a
offerClear Offer { .. } = Offer
  { offering = []
  , offerDeck = foldr RQ.discard offerDeck offering
  }

-- | Take one of the offered items, identified positionally in the offerings.
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

-- | Add an item back to the offer's deck.
offerReturnItem :: a -> Offer a -> Offer a
offerReturnItem a Offer { .. } =
  Offer { offerDeck = RQ.discard a offerDeck, .. }

-- | How many items are on offer.
offeringLength :: Offer a -> Int
offeringLength Offer { .. } = length offering


--------------------------------------------------------------------------------

-- | Setup a new card offer.
newDeedOffer :: StdGen {-^ Some randomness -}             ->
                Int    {-^ How many cards are on offer -} ->
                [Deed] {-^ The deck for this offer -}     ->
                Offer Deed
newDeedOffer r n cs = offerNewItems n (offerNothing r cs)

-- | Recycle the oldest offering and pick a fresh one, if possible.
refreshDeedOffer :: Offer Deed -> Offer Deed
refreshDeedOffer Offer { .. } =
  case reverse offering of
    []     -> Offer { .. }
    x : xs -> offerNewItem
            $ offerReturnItem x Offer { offering  = reverse xs, .. }

-- | Take the oldest offering, and pick a fresh one.
refreshDeedOfferDummy :: Offer Deed -> (Maybe Deed, Offer Deed)
refreshDeedOfferDummy Offer { .. } =
  case reverse offering of
    [] -> (Nothing, Offer { .. })
    x : xs -> (Just x, offerNewItem Offer { offering = reverse xs, .. })

-- | Take one of the cards on offer.
offerTakeDeed :: Int -> Offer Deed -> Maybe (Deed, Offer Deed)
offerTakeDeed n o =
  do (c,o1) <- offerTakeItem n o
     return (c, offerNewItem o1)

-- | Recycle the given cards.
offerReturnDeeds :: [Deed] -> Offer Deed -> Offer Deed
offerReturnDeeds cs o = foldr offerReturnItem o cs

--------------------------------------------------------------------------------

-- The unit decks.
data UnitOffer = UnitOffer
  { regularUnitOffer :: Offer Unit
  , eliteUnitOffer   :: Offer Unit
  }

unitsOnOffer :: UnitOffer -> [Unit]
unitsOnOffer UnitOffer { .. } = offering regularUnitOffer ++
                                offering eliteUnitOffer

emptyUnitOffer :: StdGen -> [Unit] -> [Unit] -> UnitOffer
emptyUnitOffer g regular elite =
  UnitOffer { regularUnitOffer = offerNothing g1 regular
            , eliteUnitOffer   = offerNothing g2 elite
            }
  where
  (g1,g2) = split g

clearUnitOffer :: UnitOffer -> UnitOffer
clearUnitOffer UnitOffer { .. } =
  UnitOffer { regularUnitOffer = offerClear regularUnitOffer
            , eliteUnitOffer   = offerClear eliteUnitOffer
            }

stockUpRegulars :: Int -> UnitOffer -> UnitOffer
stockUpRegulars n UnitOffer { .. } =
  UnitOffer { regularUnitOffer = offerNewItems n regularUnitOffer, .. }

stockUpMixed :: Int -> UnitOffer -> UnitOffer
stockUpMixed n UnitOffer { .. } =
  UnitOffer { regularUnitOffer = offerNewItems regularNum regularUnitOffer
            , eliteUnitOffer   = offerNewItems eliteNum   eliteUnitOffer
            }
  where
  regularNum = div n 2
  eliteNum   = n - regularNum

-- | Take a unit from the offer.  Regulars are indexed before elite.
offerTakeUnit :: Int -> UnitOffer -> Maybe (Unit, UnitOffer)
offerTakeUnit n UnitOffer { .. }
  | n < 0 = Nothing
  | Just (u,rs) <- offerTakeItem n regularUnitOffer =
                        Just (u, UnitOffer { regularUnitOffer = rs, .. })
  | otherwise =
    do let eliteIx = n - offeringLength regularUnitOffer
       (u,es) <- offerTakeItem eliteIx eliteUnitOffer
       return (u, UnitOffer { eliteUnitOffer = es, .. })

-- | Recycle a unit.
offerDisbandUnit :: Unit -> UnitOffer -> UnitOffer
offerDisbandUnit u UnitOffer { .. } =
  case unitType u of
    RegularUnit ->
      UnitOffer { regularUnitOffer = offerReturnItem u regularUnitOffer, ..}
    EliteUnit ->
      UnitOffer { eliteUnitOffer = offerReturnItem u eliteUnitOffer, .. }

--------------------------------------------------------------------------------

data Offers = Offers
  { advancedActionOffer :: Offer Deed
  , spellOffer          :: Offer Deed
  , unitOffer           :: UnitOffer
  , monasteryTech       :: [Deed]   -- part of unit offer
  , artifactDeck        :: ResourceQ Deed
  , activeMonasteries   :: Int
    -- ^ Revealed monasteries that have not been burnt.

  , unitNumber          :: Int
    -- ^ Number of units to load when restocking. Usually, is @playerNum + 2@.

  , commonSkillOffer    :: [Skill]
  }

data OfferSetup = OfferSetup
  { useSpellNum           :: Int
    -- ^ How many cards in the spell offer.

  , useSpells             :: [Deed]
    -- ^ Which spell cards to use.

  , useAdvancedActionNum  :: Int
    -- ^ How many cards in the advanced action offer.

  , useAdvancedActions    :: [Deed]
    -- ^ Use these advanced action cards.

  , useUnitNum            :: Int
    -- ^ How many units in the unit offer.

  , useRegularUnits       :: [Unit]
    -- ^ Use these regular units.

  , useEliteUnits         :: [Unit]
    -- ^ Use these elite units.

  , useArtifacts          :: [Deed]
    -- ^ Use this artifact deck.
  }


defaultOfferSetup :: Int {- ^ Number of players -} ->
                     Bool {- ^ Is this a coop game -} ->
                     OfferSetup
defaultOfferSetup playerNum coop = OfferSetup
  { useSpellNum = 3
  , useSpells   = if playerNum == 1 || coop
                      then filter (not . interactiveSpell) spells
                      else spells

  , useAdvancedActionNum = 3
  , useAdvancedActions  = advancedActions

  , useUnitNum      = playerNum + 2
  , useRegularUnits = regularUnits
  , useEliteUnits   = eliteUnits

  , useArtifacts    = artifacts

  }




setupOffers :: StdGen -> OfferSetup -> Offers
setupOffers r0 OfferSetup { .. } = Offers
  { advancedActionOffer = newDeedOffer randAct useAdvancedActionNum
                                               useAdvancedActions
  , spellOffer          = newDeedOffer randSpell useSpellNum useSpells

  , unitNumber          = useUnitNum
  , unitOffer           = stockUpRegulars useUnitNum
                        $ emptyUnitOffer randUnit useRegularUnits useEliteUnits

  , monasteryTech       = []
  , activeMonasteries   = 0

  , artifactDeck        = RQ.fromListRandom randArt useArtifacts

  , commonSkillOffer    = []
  }
  where
  (randAct,r1)        = split r0
  (randSpell,r2)      = split r1
  (randUnit,randArt)  = split r2

offeringAdvancedActions :: Offers -> [Deed]
offeringAdvancedActions = offering . advancedActionOffer

offeringSpells :: Offers -> [Deed]
offeringSpells = offering . spellOffer

offeringUnits :: Offers -> [Unit]
offeringUnits = unitsOnOffer . unitOffer

offeringMonasteries :: Offers -> [Deed]
offeringMonasteries = monasteryTech


takeAdvancedAction :: Int -> Offers -> Maybe (Deed, Offers)
takeAdvancedAction n Offers { .. } =
  do (card, offer) <- offerTakeDeed n advancedActionOffer
     return (card, Offers { advancedActionOffer = offer, .. })

takeSpell :: Int -> Offers -> Maybe (Deed, Offers)
takeSpell n Offers { .. } =
  do (card, offer) <- offerTakeDeed n spellOffer
     return (card, Offers { spellOffer = offer, .. })

takeUnit :: Int -> Offers -> Maybe (Unit, Offers)
takeUnit n Offers { .. } =
  do (unit , offer) <- offerTakeUnit n unitOffer
     return (unit, Offers { unitOffer = offer, .. })

takeMonasteryTech :: Int -> Offers -> Maybe (Deed, Offers)
takeMonasteryTech n Offers { .. } =
  do guard (n >= 0 && n < length monasteryTech)
     let (as,b:bs) = splitAt n monasteryTech
     return (b, Offers { monasteryTech = as ++ bs, .. })

newMonastery :: Offers -> Offers
newMonastery Offers { .. } =
  case RQ.take (offerDeck advancedActionOffer) of
    Nothing -> Offers { activeMonasteries = 1 + activeMonasteries, .. }
    Just (c,a) -> Offers { activeMonasteries = 1 + activeMonasteries
                         , advancedActionOffer =
                              advancedActionOffer { offerDeck = a }
                         , monasteryTech = c : monasteryTech
                         , ..
                         }

burnMonastery :: Offers -> Offers
burnMonastery Offers { .. } =
  Offers { activeMonasteries = max 0 (activeMonasteries - 1), .. }

disbandUnit :: Unit -> Offers -> Offers
disbandUnit u Offers { .. } =
  Offers { unitOffer = offerDisbandUnit u unitOffer, .. }


refreshUnitOffer :: Bool -> Offers -> Offers
refreshUnitOffer useElite Offers { .. } =
  Offers { unitOffer     = stockUpUnits unitNumber $ clearUnitOffer unitOffer
         , monasteryTech = offering newMons
         , advancedActionOffer = newMons { offering = saved }
         , ..
         }
  where
  stockUpUnits = if useElite then stockUpMixed else stockUpRegulars

  returned = offerReturnDeeds monasteryTech advancedActionOffer
  saved    = offering returned

  newMons  = offerNewItems activeMonasteries returned { offering = [] }


refreshOffers :: Bool {- ^ Should we use elite troops -} -> Offers -> Offers
refreshOffers useElite o0 =
  Offers { advancedActionOffer = refreshDeedOffer advancedActionOffer
         , spellOffer          = refreshDeedOffer spellOffer
         , ..
         }
  where
  Offers { .. } = refreshUnitOffer useElite o0


refreshOffersDummy :: Bool {- ^ Should we use elite troops -} ->
                      Offers ->
                      (Maybe Deed, Maybe Deed, Offers)
                      -- ^ (last advacned action, last spell, new offers)
refreshOffersDummy useElite o0 =
  ( lastAction
  , lastSpell
  , Offers { advancedActionOffer = newActions
           , spellOffer          = newSpells
           , ..
           }
  )
  where
  Offers { .. } = refreshUnitOffer useElite o0
  (lastAction,newActions) = refreshDeedOfferDummy advancedActionOffer
  (lastSpell,newSpells)   = refreshDeedOfferDummy spellOffer


-- | Add a rejected skill to the common skill offer.
newSkill :: Skill -> Offers -> Offers
newSkill s o = o { commonSkillOffer = s : commonSkillOffer o }

-- | Reject both skill, and take one from the offer.
takeSkill :: Skill -> Skill -> Int -> Offers -> Perhaps (Skill, Offers)
takeSkill s1 s2 n0 o =
  do (s,others) <- get n0 (commonSkillOffer o)
     return (s, o { commonSkillOffer = s1 : s2 : others })
  where
  get _ []        = Failed "This skill is not available."
  get n (x : xs)
    | n == 0      = Ok (x,xs)
    | n >  0      = do (y,ys) <- get (n-1) xs
                       return (y,x:ys)
    | otherwise   = Failed "This skill is not available."

--------------------------------------------------------------------------------

instance Export Offers where
  toJS o =
    object
      [ "advancedActions" .= offeringAdvancedActions o
      , "spells"          .= offeringSpells o
      , "units"           .= offeringUnits o
      , "monasteries"     .= offeringMonasteries o
      , "skills"          .= commonSkillOffer o
      ]


