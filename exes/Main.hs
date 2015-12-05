{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Common
import Offers as Offers
import Deed
import Units
import Terrain
import Game
import Land(getFeatureAt, getRevealedEnemiesAt)
import Enemies
import Player as Player
import DeedDecks(findDeed)

import Util.Perhaps
import Util.JSON
import Util.Snap

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory, serveFile)


import           Data.Char(toLower, isAlphaNum, isAscii)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Maybe(maybeToList)
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import qualified Data.Set  as Set
import           Data.List ( nub )
import           Data.IORef ( IORef, newIORef, writeIORef, atomicModifyIORef'
                            , readIORef )

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class(liftIO)

import           System.FilePath((</>), (<.>))
import           System.Random (newStdGen)


main :: IO ()
main =
  do r <- newStdGen
     s <- newIORef SnapState { snapGame = testGame r
                             , snapHistory = []
                             , snapFuture  = []
                             }

     quickHttpServe $ Snap.route
       [ ("/deed/:deed",        getDeedImage)
       , ("/unit/:unit",        getUnitImage)

       , ("/updateFame", updateFame s)
       , ("/setReputation", setReputation s)
       , ("/addCrystal",    snapAddCrystal s)
       , ("/woundUnit",     snapWoundUnit s)
       , ("/healUnit",      snapHealUnit s)
       , ("/unitToggleReady", snapUnitToggleReady s)
       , ("/disbandUnit",     snapDisbandUnit s)
       , ("/addUnitSlot",     snapAddUnitSlot s)

       , ("/drawCard",     snapDrawCard s)
       , ("/playCard",     snapPlayCard s)
       , ("/playCardFor",  snapPlayCardFor s)
       , ("/assignDamage", snapAssignDamage s)
       , ("/useCrystal",   snapUseCrystal s)
       , ("/useDie",       snapUseDie s)
       , ("/powerUp",      snapPowerUp s)
       , ("/spendMana",    snapSpendMana s)
       , ("/addPlayerDamage", snapAddPlayerDamage s)
       , ("/healPlayerWound", snapHealPlayerWound s)


       , ("/takeOffered",    takeOffered s)
       , ("/refreshOffers",  snapRefreshOffers s)
       , ("/newMonastery",   snapUpdateOffers s newMonastery)
       , ("/burnMonastery",  snapUpdateOffers s burnMonastery)

       , ("/refillSource",   snapRefillSource s)

       , ("/mapHelpUrl",     snapGetMapHelpUrl s)

       , ("/move",           snapMovePlayer s)


       , ("/undo",           snapUndo s)
       , ("/redo",           snapRedo s)


       -- testing
       , ("/newGame",                    newGame s)
       -- , ("/click",           snapClickHex s)
       ] <|> serveDirectory "ui"

data SnapState = SnapState
  { snapGame      :: Game
  , snapHistory   :: [Game]
  , snapFuture    :: [Game]
  }

type Act = IORef SnapState -> Snap ()


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------



--------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- RO

--------------------------------------------------------------------------------

newGame :: Act
newGame s = sendJSON =<< liftIO go
  where
  go = do r <- liftIO newStdGen
          let t = testGame r
          writeIORef s SnapState { snapGame    = t
                                 , snapHistory = []
                                 , snapFuture  = []
                                 }
          return t

snapUndo :: Act
snapUndo ref =
  do g <- liftIO $ atomicModifyIORef' ref $ \s ->
          case snapHistory s of
            []     -> (s, snapGame s)
            g : gs -> ( SnapState { snapFuture  = snapGame s : snapFuture s
                                  , snapGame    = g
                                  , snapHistory = gs
                                  }
                      , g
                      )
     sendJSON g

snapRedo :: Act
snapRedo ref =
  do g <- liftIO $ atomicModifyIORef' ref $ \s ->
          case snapFuture s of
            []     -> (s, snapGame s)
            g : gs -> ( SnapState { snapFuture  = gs
                                  , snapGame    = g
                                  , snapHistory = snapGame s : snapHistory s
                                  }
                      , g
                      )
     sendJSON g

snapUpdateGameMaybe :: IORef SnapState -> (Game -> Maybe Game) -> Snap ()
snapUpdateGameMaybe ref f =
  do s1 <- liftIO $ atomicModifyIORef' ref $ \s ->
        let g = snapGame s
        in (\x -> (x,x)) $
           case f g of
             Just g1 -> SnapState { snapGame    = g1
                                  , snapHistory = g : snapHistory s
                                  , snapFuture  = []
                                  }
             Nothing -> s
     sendJSON (snapGame s1)


snapUpdateGame :: IORef SnapState -> (Game -> Game) -> Snap ()
snapUpdateGame ref f = snapUpdateGameMaybe ref (Just . f)

snapUpdatePlayer :: IORef SnapState -> (Player -> Player) -> Snap ()
snapUpdatePlayer ref f = snapMaybeUpdatePlayer ref (Just . f)

snapMaybeUpdatePlayer :: IORef SnapState -> (Player -> Maybe Player) -> Snap ()
snapMaybeUpdatePlayer ref f = snapUpdateGameMaybe ref $
                              writeAttr' thePlayer f

{-
snapClickHex :: Act
snapClickHex ref =
  do addr <- addrParam
     s    <- readIORef ref
     let g = snapGame s
     sendJSON $ object [ "tag"   .= ("menu" :: Text)
                       , "items" .= [ "go"
                                    , "info"
                                    ]
                       ]
-}


takeOffered :: Act
takeOffered ref =
  do offer <- textParam "offer"
     card  <- natParam  "card"
     upd'  <- case offer of
                "advancedActions" -> takeDeed $ takeAdvancedAction card
                "spells"          -> takeDeed $ takeSpell card
                "monasteries"     -> takeDeed $ takeMonasteryTech card
                "units" -> return $ \g ->
                  do (u,o1) <- takeUnit card (offers g)
                     p1     <- hireUnit u (player g)
                     return g { offers = o1, player = p1 }
                _ -> badInput "Unknown offer"

     snapUpdateGameMaybe ref upd'
  where
  takeDeed f =
    do txt <- textParam "target"
       upd <- case txt of
                "deed"    -> return newDeed
                "discard" -> return newDiscardedDeed
                _         -> badInput "Invalid target"
       return $ \g -> do (c,o1) <- f (offers g)
                         return g { offers = o1, player = upd c (player g) }

snapUpdateOffers :: IORef SnapState -> (Offers -> Offers) -> Snap ()
snapUpdateOffers ref f = snapUpdateGame ref $ writeAttr theOffers f

snapRefreshOffers :: Act
snapRefreshOffers ref =
  do useElite <- boolParam "elite"
     snapUpdateOffers ref $ refreshOffers useElite

updateFame :: Act
updateFame ref =
  do amt <- natParam "amount"
     inc <- boolParam "increase"
     let d = if inc then amt else negate amt
     snapMaybeUpdatePlayer ref (playerAddFame d)

setReputation :: Act
setReputation ref =
  do r <- natParam "reputation"
     snapMaybeUpdatePlayer ref (playerSetReputation (r - 7))

snapAddCrystal :: Act
snapAddCrystal ref =
  do r <- basicManaParam "color"
     snapMaybeUpdatePlayer ref (addCrystal r)

snapAssignDamage :: Act
snapAssignDamage ref =
  do p <- boolParam "poison"
     d <- natParam "damage"
     snapUpdatePlayer ref (snd . assignDamage d p)


snapWoundUnit :: Act
snapWoundUnit ref =
  do u <- natParam "unit"
     snapMaybeUpdatePlayer ref (woundUnit u)

snapHealUnit :: Act
snapHealUnit ref =
  do u <- natParam "unit"
     snapMaybeUpdatePlayer ref (healUnit u)

snapUnitToggleReady :: Act
snapUnitToggleReady ref =
  do u <- natParam "unit"
     snapMaybeUpdatePlayer ref (unitToggleReady u)

snapDisbandUnit :: Act
snapDisbandUnit ref =
  do u <- natParam "unit"
     snapUpdateGameMaybe ref $ \g ->
       do (uni,p1) <- Player.disbandUnit u (player g)
          return g { player = p1
                   , offers = Offers.disbandUnit uni (offers g)
                   }

snapAddUnitSlot :: Act
snapAddUnitSlot ref = snapUpdatePlayer ref addUnitSlot

snapDrawCard :: Act
snapDrawCard ref = snapMaybeUpdatePlayer ref drawCard


snapPlayCard :: Act
snapPlayCard ref =
  do n <- natParam "card"
     snapUpdateGame ref (playCard n)

snapPlayCardFor :: Act
snapPlayCardFor ref =
  do n <- natParam "card"
     a <- textParam "action"
     act <- case a of
              "move"      -> return Movement
              "influence" -> return Influence
              "attack"    -> return Attack
              "block"     -> return Block
              _           -> badInput "Invalid action type."

     snapUpdateGame ref (playCardFor act n)

snapUseCrystal :: Act
snapUseCrystal ref =
  do c <- basicManaParam "color"
     snapUpdateGame ref (useCrystal c)


snapUseDie :: Act
snapUseDie ref =
  do c <- manaParam "color"
     snapUpdateGameMaybe ref (useDie c)

snapRefillSource :: Act
snapRefillSource ref = snapUpdateGame ref gameRefillSource


snapPowerUp :: Act
snapPowerUp ref =
  do i <- natParam "card"    -- power up this card
     snapUpdateGame ref $ writeAttr thePlayArea (powerUpCard i)


snapSpendMana :: Act
snapSpendMana ref =
  do c <- manaParam "color"
     snapUpdateGame ref $ writeAttr thePlayArea (removeManaToken c)


--------------------------------------------------------------------------------

snapMovePlayer :: Act
snapMovePlayer ref =
  do a <- addrParam
     snapUpdateGameMaybe ref $ \g ->
       isOk (if addrOnMap a g then movePlayerTo a g else explore a g)



snapAddPlayerDamage :: Act
snapAddPlayerDamage ref =
  do a <- natParam "amount"
     p <- boolParam "poison"
     snapUpdateGame ref $ writeAttr thePlayer (snd . assignDamage a p)

snapHealPlayerWound :: Act
snapHealPlayerWound ref =
  snapUpdateGameMaybe ref $ writeAttr' thePlayer (healWound WoundInHand)




