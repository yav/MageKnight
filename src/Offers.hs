module Offers
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

    -- * Artifacts
  , takeArtifacts
  , returnArtifact

    -- * Others
  , newSkill
  , newMonastery
  , burnMonastery
  , disbandUnit
  ) where

import KOI.RNGM
import Util.Q

import Offer

import Units
import Skill
import Deed.Type ( Deed(..), allDeeds )



-- | Setup a new card offer.
newDeedOffer :: Int    {-^ How many cards are on offer -} ->
                [Deed] {-^ The deck for this offer -}     ->
                Gen (Offer Deed)
newDeedOffer n cs = offerNewItems n <$> offerNew cs

-- | Recycle the oldest offering and pick a fresh one, if possible.
refreshDeedOffer :: Offer Deed -> Offer Deed
refreshDeedOffer offer =
  case offerTakeLastItem offer of
    Just (x,xs) -> offerNewItem (offerReturnItem x xs)
    Nothing     -> offer

-- | Take the oldest offering, and pick a fresh one.
refreshDeedOfferDummy :: Offer Deed -> (Maybe Deed, Offer Deed)
refreshDeedOfferDummy offer =
  case offerTakeLastItem offer of
    Just (x,xs) -> (Just x, offerNewItem xs)
    Nothing     -> (Nothing, offer)

-- | Take one of the cards on offer.
offerTakeDeed :: Int -> Offer Deed -> Maybe (Deed, Offer Deed)
offerTakeDeed n o =
  do (c,o1) <- offerTakeItem n o
     return (c, offerNewItem o1)


--------------------------------------------------------------------------------

-- The unit decks.
data UnitOffer = UnitOffer
  { regularUnitOffer :: Offer Unit
  , eliteUnitOffer   :: Offer Unit
  }

unitsOnOffer :: UnitOffer -> [Unit]
unitsOnOffer UnitOffer { .. } = offerItems regularUnitOffer ++
                                offerItems eliteUnitOffer

emptyUnitOffer :: [Unit] -> [Unit] -> Gen UnitOffer
emptyUnitOffer regular elite =
  do regularUnitOffer <- offerNew regular
     eliteUnitOffer   <- offerNew elite
     return UnitOffer { .. }

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
    do let eliteIx = n - offerLength regularUnitOffer
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
  , artifactDeck        :: Q Deed
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
                     OfferSetup
defaultOfferSetup playerNum = OfferSetup
  { useSpellNum = 3
  , useSpells   = allDeeds Spell

  , useAdvancedActionNum = 3
  , useAdvancedActions  = allDeeds AdvancedAction

  , useUnitNum      = playerNum + 2
  , useRegularUnits = regularUnits
  , useEliteUnits   = eliteUnits

  , useArtifacts    = allDeeds Artifact

  }




setupOffers :: OfferSetup -> Gen Offers
setupOffers OfferSetup { .. } =
  do advancedActionOffer <- newDeedOffer useAdvancedActionNum useAdvancedActions
     spellOffer          <- newDeedOffer useSpellNum useSpells
     emptyUnit           <- emptyUnitOffer useRegularUnits useEliteUnits
     return Offers
      { unitNumber          = useUnitNum
      , unitOffer           = stockUpRegulars useUnitNum emptyUnit

      , monasteryTech       = []
      , activeMonasteries   = 0

      , commonSkillOffer    = []
      , artifactDeck        = qFromList useArtifacts

      , ..
      }

offeringAdvancedActions :: Offers -> [Deed]
offeringAdvancedActions = offerItems . advancedActionOffer

offeringSpells :: Offers -> [Deed]
offeringSpells = offerItems . spellOffer

offeringUnits :: Offers -> [Unit]
offeringUnits = unitsOnOffer . unitOffer

offeringMonasteries :: Offers -> [Deed]
offeringMonasteries = monasteryTech


-- | Take the given number of artifacts.  If there aren't enough,
-- then return whatever is available.
takeArtifacts ::  Int -> Offers -> ([Deed], Offers)
takeArtifacts n Offers { .. } =
  case qTakeFrontN n artifactDeck of
    Just (as,q) -> (as, Offers { artifactDeck = q, .. })
    Nothing     -> (qToList artifactDeck, Offers { artifactDeck = qEmpty, .. })

-- | Place this artifact at the bottom of the artifcat deck.
returnArtifact :: Deed -> Offers -> Offers
returnArtifact d Offers { .. } =
  Offers { artifactDeck = qAddBack d artifactDeck, .. }


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
  case splitAt n monasteryTech of
    (as,b:bs) -> pure (b, Offers { monasteryTech = as ++ bs, .. })
    _         -> Nothing

newMonastery :: Offers -> Offers
newMonastery Offers { .. } =
  case offerTakeFromDraw advancedActionOffer of
    Nothing -> Offers { activeMonasteries = 1 + activeMonasteries, .. }
    Just (c,a) ->
      Offers
        { activeMonasteries = 1 + activeMonasteries
        , advancedActionOffer = a
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
         , monasteryTech = newMons
         , advancedActionOffer = newAdv
         , ..
         }
  where
  stockUpUnits = if useElite then stockUpMixed else stockUpRegulars
  (newMons, newAdv) = offerTakeManyFromDraw activeMonasteries
                            (offerReturnMany monasteryTech advancedActionOffer)



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
takeSkill :: Skill -> Skill -> Int -> Offers -> Maybe (Skill, Offers)
takeSkill s1 s2 n0 o =
  do (s,others) <- get n0 (commonSkillOffer o)
     return (s, o { commonSkillOffer = s1 : s2 : others })
  where
  get _ []        = Nothing
  get n (x : xs)
    | n == 0      = Just (x,xs)
    | n >  0      = do (y,ys) <- get (n-1) xs
                       return (y,x:ys)
    | otherwise   = Nothing

--------------------------------------------------------------------------------

{-
instance Export Offers where
  toJS o =
    object
      [ "advancedActions" .= offeringAdvancedActions o
      , "spells"          .= offeringSpells o
      , "units"           .= offeringUnits o
      , "monasteries"     .= offeringMonasteries o
      , "skills"          .= commonSkillOffer o
      ]

-}
