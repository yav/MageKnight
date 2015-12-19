{-# LANGUAGE Safe, RecordWildCards, OverloadedStrings #-}
module Combat where

import Common
import Enemies
import Util.Perhaps
import Util.Bag
import Deed
import Player
import Land
import Terrain

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set ( Set )
import qualified Data.Set as Set
import           Data.Foldable (any, foldr)
import           Data.Either(partitionEithers)
import           Data.Maybe(isNothing)
import           Data.List(partition,nub)
import           Prelude hiding (any, foldr)
import           qualified Data.Text as Text
import           Control.Monad(when)


--------------------------------------------------------------------------------

-- | Why are we fighting?
data CombatReason =
    AttackFortified FortifiedType
                     -- ^ We are attacking a fortified site (e.g., keep)
  | AttackAdventure  -- ^ We are attacking an adventure site (e.g. dungeon)
  | BurnMonastery    -- ^ Oh no!
  | AttackRampaging  -- ^ We are attacking a rampaging enemy (e.g. orcs)
  | ProvokeRampaging -- ^ We are being attacked by provoked rampaging enemy.

data FortifiedType = AttackConqueredKeep
                     -- ^ Fame is worth only half;
                     -- enemy disappears ef not defeated.
                   | AttackUnconquered

-- | For some fights, we are also allowed to challenge rampaging enemies
-- adjecent to the combat site.
-- As it happens, these are exactly the "forced" combat.
mayAddRampagingNeighbours :: CombatReason -> Bool
mayAddRampagingNeighbours reason =
  case reason of
    AttackFortified _ -> True
    ProvokeRampaging  -> True
    _                 -> False

{-
determineCombatReason :: Player -> Land -> Perhaps CombatReason
determineCombatReason p l =
  case getFeatureAt addr l of
    Nothing -> Failed "You may not start combat at this location."
    Just (terra, maybeFeat) ->
      case terra of
        City col ->

  where
  addr = playerLocation p
-}



-- | An enemy in action.  Used while we are fighting it.
data ActiveEnemy = ActiveEnemy
  { enemyId         :: Int    -- ^ Used to distinguish enemies during a combat
  , enemyFortLoc    :: Bool   -- ^ In a fortified location

  , enemyInCity     :: Maybe BasicMana
    -- ^ Is this enemy in a city.  Used when we activate summoned enemies.

  , enemyOrigStats  :: Enemy
    -- ^ Original stats of the enemy.
    -- Some abilities refer to the stats printed on the actual token.
    -- (e.g. cold toughness gives bonuses only for what's on the token)

  , enemyStats      :: Enemy
    -- ^ Current enemy stats, which may include various bonuses
    -- (e.g., due to city bonuses), or be updated by casting spells
    -- on the enemy.

  , isRampaging     :: Bool -- ^ Is this a rampaging enemy?

  , enemyLoc        :: Addr -- ^ Location on the board.

  , enemyLifeSpan   :: EnemyLifeSpan

  }


{- Combat outcome:
  undefeated enemies disappear:
    - dungeion, tomb, monastery, conquered keep protector(?)

  undefeated stay:
    - city, unconquered keep, unconquered mage tower,
      monster den, spawning grounds, ruins

-}


-- | Compute the active stats for an enemy, depending on the city they are in
activeStats :: Maybe BasicMana -> Enemy -> Enemy
activeStats mb e =
  case mb of
    Nothing -> e
    Just city ->
      case city of
        White -> e { enemyArmor = enemyArmor e + 1 }
        Blue  -> case enemyAttack e of
                   Summoner -> e
                   AttacksWith el n ->
                     case el of
                        Physycal -> e
                        Fire     -> e { enemyAttack = AttacksWith el (n+2) }
                        Ice      -> e { enemyAttack = AttacksWith el (n+2) }
                        ColdFire -> e { enemyAttack = AttacksWith el (n+1) }
        Red   -> case enemyAttack e of
                   AttacksWith Physycal _ ->
                     e { enemyAbilities = Set.insert Brutal (enemyAbilities e) }
                   _ -> e
        Green ->
          case enemyAttack e of
            AttacksWith Physycal _ ->
              e { enemyAbilities = Set.insert Poisons (enemyAbilities e) }
            _ -> e

enemyHasAbility :: EnemyAbility -> ActiveEnemy -> Bool
enemyHasAbility x = Set.member x . enemyAbilities . enemyStats

enemyResists :: Element -> ActiveEnemy -> Bool
enemyResists ColdFire e = enemyHasAbility (Resists Ice) e &&
                          enemyHasAbility (Resists Fire) e
enemyResists x e        = enemyHasAbility (Resists x) e

isSummoner :: ActiveEnemy -> Bool
isSummoner e =
  case enemyAttack (enemyStats e) of
    Summoner -> True
    _        -> False

isSummoned :: ActiveEnemy -> Bool
isSummoned e =
  case enemyLifeSpan e of
    EnemySummoned -> True
    _             -> False

isBlockPhase :: CombatPhaseType -> Bool
isBlockPhase ph =
  case ph of
    BlockPhase _ -> True
    _            -> False


instance Eq ActiveEnemy where
  x == y = enemyId x == enemyId y

instance Ord ActiveEnemy where
  compare x y = compare (enemyId x) (enemyId y)




--------------------------------------------------------------------------------




--------------------------------------------------------------------------------

-- | Used for ranged attack, blockinga, and melee attacks.
data CombatPhase = CombatPhase
  { currentSkirmish   :: Maybe Skirmish
    -- ^ What we are attacking/blocking currently.

  , combatPhase       :: CombatPhaseType
    -- ^ What phase are we working on.

  , remainingEnemies  :: Map Int ActiveEnemy
    -- ^ Enemies available for attack/block.
    -- Those have been removed from the land and reside here.

  , deadEnemies       :: [Enemy]
    -- ^ Enemies that were killed.  These have been returned to the land.
    -- We keep them here so that we can score them at the end.
    -- Does not contain summoned enemies.

  , usedDeeds         :: [Deed]
    -- ^ Deeds that were used during combat, but are not active any more.
    -- These should be returned to the player at the end of combat.

  , nextActiveId      :: !Int
    -- ^ Used when we active enemies, to generate identities.

  , combatPlayer      :: Player
    -- ^ The player conducting the combat.

  , combatLand        :: Land
    -- ^ The state of the land.  Mostly used for generating/discarding
    -- enemies.

  , combatReason      :: CombatReason
    -- ^ We are we fighting.  Used to figure out what happens at the
    -- end of comabt.

  , rampagingSites    :: [Addr]
    -- ^ The combat started with participants from these rampaging sites.
    -- The location of the player may be used to determine what, if any,
    -- are the enemies for an fortified or advanture site.

  }


-- | An attack of some enemies, or a block of a single enmy.
data Skirmish = Skirmish
  { skirmishEnemies   :: Map Int ActiveEnemy
    -- ^ Who is being attacked or blocked, indexed by id.
    -- Never empty.  Always singleton during the blocking phase.

  , playerActions     :: [ActiveDeed]
    -- ^ Deeds to resolve the attack.  Most recently played at the front.
  }



-- | Combat phases that require playing cards
data CombatPhaseType = RangedAttackPhase
                     | BlockPhase [ActiveEnemy] -- ^ These have been blocked
                     | AssignDamage Damage
                     | MeleeAttackPhase

-- | Current state of damage assignment.
data Damage = Damage
  { damageTodo    :: [ActiveEnemy]  -- ^ More enemies that hurt us
  , damageCurrent :: Maybe (ActiveEnemy,Int,DamageInfo)
    -- ^ Current enemy that is hurting us and remaing amount and damage type.
  , damageDone    :: [ActiveEnemy]
  }


-- | Determine the amount and type of damage done by an enemy.
computeDamage :: ActiveEnemy -> (Int, DamageInfo)
computeDamage e =
  case enemyAttack (enemyStats e) of
    Summoner -> error "[bug] computing damage of a summoner"
    AttacksWith el baseAmt ->
      ( if enemyHasAbility Brutal e then baseAmt * 2 else baseAmt
      , DamageInfo { damageElement   = el
                   , damagePoisons   = enemyHasAbility Poisons e
                   , damageParalyzes = enemyHasAbility Paralyzes e
                   }
      )

-- | We've assigned all damage that we needed to.
noMoreDamage :: Damage -> Bool
noMoreDamage Damage { .. } = null damageTodo && isNothing damageCurrent

-- | Decrease some of the damage dealt by the current enemy.
-- If we go down to 0 or less, then there is no current enemy.
decreaseDamage :: Int -> Damage -> Perhaps Damage
decreaseDamage n Damage { .. } =
  do (a,m,i) <- perhaps "No enemy is currently attacking" damageCurrent
     return $
       if n >= m
         then Damage { damageDone = a : damageDone
                     , damageCurrent = Nothing, .. }
         else Damage { damageCurrent = Just (a,m-n,i), .. }

-- | Choose another enemy to assign damage from.
pickAttacker :: Int -> Damage -> Perhaps Damage
pickAttacker n Damage { .. }
  | Nothing <- damageCurrent =
    case break ((n ==) . enemyId) damageTodo of
      (as,b:bs) ->
        let (amt,info) = computeDamage b
        in return Damage { damageTodo = as ++ bs
                         , damageCurrent = Just (b, amt, info)
                         , .. }
      _ -> Failed "No such attacker."
  | otherwise = Failed "Current enemy attack is not resolved yet."


-- | A skirmish with the given enemies.
newSkirmish :: [ActiveEnemy] -> Skirmish
newSkirmish es = Skirmish { skirmishEnemies = Map.fromList (map prep es)
                          , playerActions   = []
                          }
  where prep e = (enemyId e, e)




-- | Start a combat, entering the ranged attack phase.
startCombat :: Player -> Land -> CombatReason -> [(Addr,Enemy)] -> CombatPhase
startCombat p l reason es =
  CombatPhase { currentSkirmish  = Nothing
              , remainingEnemies = Map.fromList activated
              , deadEnemies      = []
              , usedDeeds        = []
              , combatPhase      = RangedAttackPhase
              , combatPlayer     = p
              , combatLand       = l
              , nextActiveId     = length es
              , combatReason     = reason
              , rampagingSites   = nub
                                 $ map enemyLoc
                                 $ filter isRampaging
                                 $ map snd activated
              }
  where
  activated = zipWith prep [0..] es
  prep = undefined


{-
  -- Activate an enemy
  prep n (a,e) =
    let unexpected = (False, (False, False, LivesPastCombat))
        (inCity, (inFort, rampTribe, lifeSpan)) =
          case getFeatureAt a l of
            Nothing     -> unexpected
            Just (t,mb) ->
              case t of
                City c -> (Just c, (True,False, LivesPastCombat))
                _      -> (Nothing, case mb of
                                      Nothing -> (False, False)
                                      Just f ->
                                        case f of
                                          Keep              -> (True, False)
                                          MageTower         -> (True, False, LifeSpan)
                                          Dungeon           -> (False, False)
                                          Tomb              -> (False, False)
                                          MonsterDen        -> (False, False, )
                                          SpawningGrounds   -> (False, False,LivesPastCombat)
                                          AncientRuins      -> (False, False, LivesPastCombat)
                                          RampagingEnemy _  -> (False, True, LivesPastCombat)
                                          MagicalGlade      -> unexpected
                                          Mine _            -> unexpected
                                          Village           -> unexpected
                                          Monastery         -> (False, False, LivesForCombatOnly))
    in (n, ActiveEnemy { enemyId        = n
                       , enemyFortLoc   = inFort
                       , enemyInCity    = inCity
                       , enemyOrigStats = e
                       , enemyStats     = activeStats inCity e
                       , isRampaging    = rampTribe
                       , enemyLoc       = a
                       , enemyLifeSpan  = lifeSpan
                       }) -}

-- | Finish the current skirmish.
winSkirmish :: CombatPhase -> Perhaps CombatPhase
winSkirmish CombatPhase { .. } =
  perhaps "Not skirmishing." $
  do s <- currentSkirmish

     let deeds   = map baseDeed (playerActions s)
         enemies = Map.elems (skirmishEnemies s)
         (summoned,normal) = partition isSummoned enemies

     case combatPhase of

       BlockPhase bs ->
        return CombatPhase
                { combatPhase     = BlockPhase (normal ++ bs)
                , currentSkirmish = Nothing
                , usedDeeds       = deeds ++ usedDeeds
                , combatLand      = foldr discardEnemy combatLand
                                  $ map enemyOrigStats summoned
                , .. }

       AssignDamage _ -> error "[Bug] Skirmish while assigning damage."

       _  -> return CombatPhase
                     { currentSkirmish = Nothing
                     , deadEnemies = map enemyOrigStats normal ++ deadEnemies
                     , usedDeeds   = deeds ++ usedDeeds
                     , combatLand  = foldr discardEnemy combatLand
                                   $ map enemyOrigStats enemies
                     , .. }

-- | Attack or block a new group of enemies.
startSkirmish :: [Int] -> CombatPhase -> Perhaps CombatPhase
startSkirmish xs CombatPhase { .. }

  | Just _ <- currentSkirmish =
    Failed "Already in a skirmish."

  | [] <- xs =
    Failed "Cannot start an empty skirmish."

  | BlockPhase _ <- combatPhase, _ : _ : _ <- xs =
    Failed "Enemies need to be blocked one at a time."

  | otherwise =
    do es <- mapM lkp xs
       when (isBlockPhase combatPhase && isSummoner (head es))
         $ Failed "Summoners do not attack directly and need not be blocked."
       return CombatPhase
          { currentSkirmish  = Just (newSkirmish es)
          , remainingEnemies = foldr Map.delete remainingEnemies xs
          , .. }
  where lkp x = perhaps ("Unknown enemy: " `Text.append` Text.pack (show x))
              $ Map.lookup x remainingEnemies

-- | Play a card towards the current skirmish.
makeAttack :: ActiveDeed -> CombatPhase -> Perhaps CombatPhase
makeAttack d CombatPhase { .. } =
  do s <- perhaps "Not skirmishing." currentSkirmish
     return CombatPhase
             { currentSkirmish = Just s { playerActions = d : playerActions s }
             , .. }

-- | Proceed to the next phase of combat.
nextPhase :: CombatPhase -> Perhaps CombatPhase
nextPhase CombatPhase { .. }

  | Just _ <- currentSkirmish =
    Failed "A skirmish is currently in progress."

  | otherwise =
    case combatPhase of

      RangedAttackPhase ->
        let (summoned, newLand, newId) =
                foldr summonBy ([], combatLand, nextActiveId) $
                filter isSummoner (Map.elems remainingEnemies)
        in return CombatPhase { combatPhase   = BlockPhase []
                              , combatLand    = newLand
                              , nextActiveId  = newId
                              , remainingEnemies =
                                  foldr addActive remainingEnemies summoned
                              , .. }

      BlockPhase bs ->
        return CombatPhase
                 { combatPhase =
                     AssignDamage
                       Damage { damageTodo = rest
                              , damageDone = sus ++ bs
                              , damageCurrent = Nothing
                              }
                 , .. }
        where (sus,rest) = partition isSummoner $ Map.elems remainingEnemies

      AssignDamage d | noMoreDamage d ->
        let (su,rest) = Map.partition isSummoned remainingEnemies
            es        = map enemyOrigStats (Map.elems su)
        in return CombatPhase
             { combatPhase      = MeleeAttackPhase
             , remainingEnemies = foldr addActive rest (damageDone d)
             , combatLand       = foldr discardEnemy combatLand es
             , .. }

      AssignDamage _ -> Failed "More damage needs to be assigned"

      MeleeAttackPhase ->
        Failed "Mellee attack is the last combat phase."

  where
  addActive a mp = Map.insert (enemyId a) a mp

  summonBy s (es,l,n) =
    case summonCreature l of
      Failed _ -> (es,l,n) -- We run out of tokens, unlikely.
      Ok (e,l1) ->
        let a = ActiveEnemy
                  { enemyId         = n
                  , enemyFortLoc    = enemyFortLoc s
                  , enemyInCity     = enemyInCity s
                  , enemyOrigStats  = e
                  , enemyStats      = activeStats (enemyInCity s) e
                  , enemyLifeSpan   = EnemySummoned
                  , isRampaging     = False
                  , enemyLoc        = enemyLoc s
                  }
            n1 = n + 1
        in n1 `seq` (a:es, l1, n1)


--------------------------------------------------------------------------------


-- | Check if the attacks are sufficient to defeat the set of enemies.
successfulRangedAttack :: Bag Element {-^ Ranged -} ->
                          Bag Element {-^ Siege  -} ->
                          Set ActiveEnemy -> Bool
successfulRangedAttack rangedAttack siegeAttack enemies =
  successfulAttack attackTypes enemies
  where
  inFort    = any enemyFortLoc enemies
  fortified = any (enemyHasAbility Fortified) enemies

  attackTypes
    | inFort    = if fortified then bagEmpty else siegeAttack
    | fortified = siegeAttack
    | otherwise = bagUnion rangedAttack siegeAttack

-- | Check if the attacks are sufficient to defeat the set of enemies.
successfulAttack :: Bag Element -> Set ActiveEnemy -> Bool
successfulAttack attackTypes enemies = totalAttack >= foldr addArmor 0 enemies
  where
  addArmor e tot          = enemyArmor (enemyStats e) + tot
  resists x               = any (enemyResists x) enemies

  classify (el,amt)       = if resists el then Left amt else Right amt
  (inefficient,efficient) = partitionEithers $ map classify
                                             $ bagToListGrouped attackTypes
  totalAttack             = sum (div (sum inefficient) 2 : efficient)


-- | Check if the given amounts of block are sufficient to block an enemy.
successfulBlock :: Bag Element -> ActiveEnemy -> Bool
successfulBlock blocks enemy =
  case enemyAttack (enemyStats enemy) of
    AttacksWith ty amt -> sum (div (sum inefficient) 2 : efficient) >= attack
      where
      attack
        | enemyHasAbility Swift enemy = 2 * amt
        | otherwise                   = amt

      (inefficient,efficient) = partitionEithers $ map classify
                                                 $ bagToListGrouped blocks
      classify (bt,a) = if isEfficient bt then Right a else Left a
      isEfficient bl = case ty {- type of attack -} of
                         Physycal -> True
                         Fire     -> bl == Ice  || bl == ColdFire
                         Ice      -> bl == Fire || bl == ColdFire
                         ColdFire -> bl == ColdFire

    Summoner -> error "[Bug] Tried to block a summoner"



