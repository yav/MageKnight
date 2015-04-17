{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MageKnight.Common
import MageKnight.Offers as Offers
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Terrain
import MageKnight.JSON
import MageKnight.Game
import MageKnight.Player as Player
import MageKnight.DeedDecks(findDeed)

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory, serveFile)


import           Data.Char(toLower, isAlphaNum, isAscii)
import           Data.Maybe ( fromMaybe )
import           Data.List(find)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import           Data.IORef ( IORef, newIORef, writeIORef, readIORef
                            , atomicModifyIORef' )

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class(liftIO)

import           System.FilePath((</>), (<.>))
import           System.Random (newStdGen)


main :: IO ()
main =
  do r <- newStdGen
     s <- newIORef (testGame r)
     o <- newIORef (setupOffers r (defaultOfferSetup 1 True))

     quickHttpServe $ Snap.route
       [ ("/deed/:deed",  getDeedImage)
       , ("/unit/:unit",  getUnitImage)

       , ("/updateFame", updateFame s)
       , ("/setReputation", setReputation s)
       , ("/addCrystal",    snapAddCrystal s)
       , ("/removeCrystal", snapRemoveCrystal s)
       , ("/woundUnit",     snapWoundUnit s)
       , ("/healUnit",      snapHealUnit s)
       , ("/unitToggleReady", snapUnitToggleReady s)
       , ("/disbandUnit",     snapDisbandUnit s)
       , ("/addUnitSlot",     snapAddUnitSlot s)

       , ("/drawCard",     snapDrawCard s)

       , ("/takeOffered",    takeOffered s)
       , ("/refreshOffers",  snapRefreshOffers s)
       , ("/newMonastery",   snapUpdateOffers s newMonastery)
       , ("/burnMonastery",  snapUpdateOffers s burnMonastery)


       -- testing
       , ("/newGame",                    newGame s)
       , ("/click", clickHex s)
       ] <|> serveDirectory "ui"


--------------------------------------------------------------------------------

badInput :: ByteString -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400 msg Snap.emptyResponse)

notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)

--------------------------------------------------------------------------------

requiredParam :: ByteString -> Snap ByteString
requiredParam p =
  do mb <- Snap.getParam p
     case mb of
       Just x  -> return x
       Nothing -> badInput ("Missing parameter: " `BS.append` p)

textParam :: ByteString -> Snap Text
textParam p = decodeUtf8 `fmap` requiredParam p

intParam :: ByteString -> Snap Int
intParam p =
  do txt <- textParam p
     case decimal txt of
       Right (a,t) | Text.null t -> return a
       _ -> badInput ("Malformed integer parameter: " `BS.append` p)

boolParam :: ByteString -> Snap Bool
boolParam p =
  do bs <- requiredParam p
     case bs of
       "true"  -> return True
       "false" -> return False
       _       -> badInput ("Malformed boolean parameter: " `BS.append` p)

addrParam :: Snap Addr
addrParam =
  do x <- intParam  "tile_x"
     y <- intParam  "tile_y"
     h <- textParam "hex"
     let ok d = return (hexAddr d)
     loc <- case h of
              "NW" -> ok NW
              "NE" -> ok NE
              "W"  -> ok W
              "C"  -> ok Center
              "E"  -> ok E
              "SW" -> ok SW
              "SE" -> ok SE
              _    -> badInput "Malformed parameter: hex"
     return Addr { addrGlobal = (x,y), addrLocal = loc }

deedParam :: ByteString -> Snap Deed
deedParam pname =
  do x <- textParam pname
     case findDeed x of
       Just d  -> return d
       Nothing -> badInput (BS.append "Invalid deed parameter: " pname)

unitParam :: ByteString -> Snap Unit
unitParam pname =
  do x <- textParam pname
     case findUnit x of
       Just d  -> return d
       Nothing -> badInput (BS.append "Invalid unit parameter: " pname)

basicManaParam :: ByteString -> Snap BasicMana
basicManaParam pname =
  do x <- textParam pname
     case x of
       "red"    -> return Red
       "green"  -> return Green
       "blue"   -> return Blue
       "white"  -> return White
       _        -> badInput (BS.append "Inavlid basic mana: " pname)

sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)


--------------------------------------------------------------------------------


nameToPath :: Text -> String
nameToPath = Text.unpack . Text.map cvt
  where
  cvt c | isAscii c && isAlphaNum c = toLower c
  cvt _                             = '_'

deedPath :: Deed -> FilePath
deedPath Deed { .. } = dir </> nameToPath deedName <.> "png"
  where
  dir = case deedType of
          Wound            -> pref
          Action c         -> pref </> "basic_actions" </> color c
          AdvancedAction c -> pref </> "advanced_actions" </> color c
          Spell c          -> pref </> "spells" </> color c
          Artifact         -> pref </> "artifacts"
  pref = "ui" </> "img" </> "cards"
  color c = case c of
              Red   -> "red"
              Blue  -> "blue"
              Green -> "green"
              White -> "white"

unitPath :: Unit -> FilePath
unitPath Unit { .. } = "ui" </> "img" </> "units" </> ty </> name <.> "png"
  where ty = case unitType of
               RegularUnit -> "regular"
               EliteUnit   -> "elite"
        name = nameToPath unitName

--------------------------------------------------------------------------------

newGame :: IORef Game -> Snap ()
newGame s = sendJSON =<< liftIO go
  where
  go = do r <- liftIO newStdGen
          let t = testGame r
          writeIORef s t
          return t


clickHex :: IORef Game -> Snap ()
clickHex s = return () {-
  do a <- addrParam
     g <- liftIO (atomicModifyIORef' s (go a))
     sendJSON g
  where
  go a g = undefined
    | Just d <- find ((a ==) . neighbour loc) allDirections =
      let g1 = if addrOnMap a g then movePlayer d g
               else fromMaybe g (explore a g)
      in (g1,g1)

    | otherwise = (g,g)

      where loc = playerLocation p
            p = player g
-}

getDeedImage :: Snap ()
getDeedImage =
  do deed <- deedParam "deed"
     serveFile (deedPath deed)


getUnitImage :: Snap ()
getUnitImage =
  do unit <- unitParam "unit"
     serveFile (unitPath unit)

takeOffered :: IORef Game -> Snap ()
takeOffered ref =
  do offer <- textParam "offer"
     card  <- intParam  "card"
     upd   <- case offer of
                "advancedActions" -> takeDeed $ takeAdvancedAction card
                "spells"          -> takeDeed $ takeSpell card
                "monasteries"     -> takeDeed $ takeMonasteryTech card
                "units" -> return $ \g ->
                  fromMaybe g $
                  do (u,o1) <- takeUnit card (offers g)
                     p1     <- hireUnit u (player g)
                     return g { offers = o1, player = p1 }
                _   -> badInput "Unknown offer"
     g <- liftIO (atomicModifyIORef' ref (fork . upd))
     sendJSON g

  where
  fork x = (x,x)

  takeDeed f =
    do txt <- textParam "target"
       upd <- case txt of
                "deed"    -> return newDeed
                "discard" -> return newDiscardedDeed
                _ -> badInput "Invalid target"
       return $ \g ->
          case f (offers g) of
            Nothing     -> g
            Just (c,o1) -> g { offers = o1, player = upd c (player g) }

snapUpdateOffers :: IORef Game -> (Offers -> Offers) -> Snap ()
snapUpdateOffers ref f =
  do g1 <- liftIO $ atomicModifyIORef' ref $ \g ->
            case updateOffers (Just . f) g of
              Nothing -> (g,g)
              Just g1 -> (g1,g1)
     sendJSON (offers g1)



snapRefreshOffers :: IORef Game -> Snap ()
snapRefreshOffers ref =
  do useElite <- boolParam "elite"
     snapUpdateOffers ref $ refreshOffers useElite

snapUpdatePlayer :: IORef Game -> (Player -> Player) -> Snap ()
snapUpdatePlayer ref f =
  do g1 <- liftIO $ atomicModifyIORef' ref $ \g ->
           case updatePlayer (Just . f) g of
             Just g1 -> (g1,g1)
             Nothing -> (g,g)
     sendJSON (player g1)


updateFame :: IORef Game -> Snap ()
updateFame ref =
  do amt <- intParam "amount"
     inc <- boolParam "increase"
     let d = if inc then amt else negate amt
     snapUpdatePlayer ref (playerAddFame d)

setReputation :: IORef Game -> Snap ()
setReputation ref =
  do r <- intParam "reputation"
     snapUpdatePlayer ref (playerSetReputation (r - 7))

snapRemoveCrystal :: IORef Game -> Snap ()
snapRemoveCrystal ref =
  do r <- basicManaParam "color"
     snapUpdatePlayer ref (\p -> case removeCrystal r p of
                                   Just p1 -> p1
                                   Nothing -> p)


snapAddCrystal :: IORef Game -> Snap ()
snapAddCrystal ref =
  do r <- basicManaParam "color"
     snapUpdatePlayer ref (addCrystal r)


snapWoundUnit :: IORef Game -> Snap ()
snapWoundUnit ref =
  do u <- intParam "unit"
     snapUpdatePlayer ref (woundUnit u)

snapHealUnit :: IORef Game -> Snap ()
snapHealUnit ref =
  do u <- intParam "unit"
     snapUpdatePlayer ref (healUnit u)

snapUnitToggleReady :: IORef Game -> Snap ()
snapUnitToggleReady ref =
  do u <- intParam "unit"
     snapUpdatePlayer ref (unitToggleReady u)

snapDisbandUnit :: IORef Game -> Snap ()
snapDisbandUnit ref =
  do u <- intParam "unit"
     p <- liftIO $ atomicModifyIORef' ref $ \g ->
           fork $
           fromMaybe g $
              do (uni,p1) <- Player.disbandUnit u (player g)
                 return g { player = p1
                          , offers = Offers.disbandUnit uni (offers g)
                          }
     sendJSON p
  where
  fork x = (x, player x)

snapAddUnitSlot :: IORef Game -> Snap ()
snapAddUnitSlot ref =
  snapUpdatePlayer ref addUnitSlot

snapDrawCard :: IORef Game -> Snap ()
snapDrawCard ref =
  snapUpdatePlayer ref drawCard
