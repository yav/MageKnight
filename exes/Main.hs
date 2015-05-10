{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MageKnight.Common hiding (Resource(..))
import MageKnight.Offers as Offers
import MageKnight.Deed
import MageKnight.Units
import MageKnight.Terrain
import MageKnight.JSON
import MageKnight.Game
import MageKnight.Land(getFeatureAt)
import MageKnight.Enemies(EnemyType(..))
import MageKnight.Player as Player
import MageKnight.DeedDecks(findDeed)
import MageKnight.Loc
import MageKnight.Perhaps

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory, serveFile)


import           Data.Char(toLower, isAlphaNum, isAscii)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text
import           Data.IORef ( IORef, newIORef, writeIORef, atomicModifyIORef'
                            , readIORef )

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class(liftIO)

import           System.FilePath((</>), (<.>))
import           System.Random (newStdGen)


main :: IO ()
main =
  do r <- newStdGen
     s <- newIORef SnapState { snapGame = testGame r, snapHistory = [] }

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

       , ("/takeOffered",    takeOffered s)
       , ("/refreshOffers",  snapRefreshOffers s)
       , ("/newMonastery",   snapUpdateOffers s newMonastery)
       , ("/burnMonastery",  snapUpdateOffers s burnMonastery)

       , ("/refillSource",   snapRefillSource s)

       , ("/mapHelpUrl",     snapGetMapHelpUrl s)

       , ("/move",           snapMovePlayer s)

       -- testing
       , ("/newGame",                    newGame s)
       , ("/click",           snapClickHex s)
       ] <|> serveDirectory "ui"

data SnapState = SnapState
  { snapGame      :: Game
  , snapHistory   :: [Game]
  }

type Act = IORef SnapState -> Snap ()


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
     let (neg,numTxt) = case Text.uncons txt of
                          Just ('-',t) -> (negate, t)
                          _            -> (id, txt)
     case decimal numTxt of
       Right (a,t) | Text.null t -> return (neg a)
       _ -> badInput ("Malformed integer parameter: " `BS.append` p)

natParam :: ByteString -> Snap Int
natParam p =
  do txt <- textParam p
     case decimal txt of
       Right (a,t) | Text.null t -> return a
       _ -> badInput ("Malformed natural parameter: " `BS.append` p)



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
     lo <- case h of
             "NW" -> ok NW
             "NE" -> ok NE
             "W"  -> ok W
             "C"  -> ok Center
             "E"  -> ok E
             "SW" -> ok SW
             "SE" -> ok SE
             _    -> badInput "Malformed parameter: hex"
     return Addr { addrGlobal = (x,y), addrLocal = lo }

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

manaParam :: ByteString -> Snap Mana
manaParam pname =
  do x <- textParam pname
     case x of
       "red"    -> return (BasicMana Red)
       "green"  -> return (BasicMana Green)
       "blue"   -> return (BasicMana Blue)
       "white"  -> return (BasicMana White)
       "gold"   -> return Gold
       "black"  -> return Black
       _        -> badInput (BS.append "Inavlid mana: " pname)



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

featureHelpUrl :: Feature -> Maybe FilePath
featureHelpUrl f =
  case f of
    MagicalGlade    -> img "magical_glade"
    Mine _          -> img "mines"
    Village         -> img "village"
    Monastery       -> img "monastery"
    Keep            -> img "keep"
    MageTower       -> img "mage_tower"
    Dungeon         -> img "dungeon"
    Tomb            -> img "tomb"
    MonsterDen      -> img "monster_den"
    SpawningGrounds -> img "spawning_grounds"
    AncientRuins    -> img "ancient_ruins"
    RampagingEnemy e ->
      case e of
        Orc      -> img "marauding_orcs"
        Draconum -> img "draconum"
        _        -> Nothing

  where
  img x = Just ("img" </> "manual" </> "features" </> x <.> "png")



--------------------------------------------------------------------------------
-- RO

getDeedImage :: Snap ()
getDeedImage =
  do deed <- deedParam "deed"
     serveFile (deedPath deed)


getUnitImage :: Snap ()
getUnitImage =
  do unit <- unitParam "unit"
     serveFile (unitPath unit)

snapGetMapHelpUrl :: Act
snapGetMapHelpUrl ref =
  do a  <- addrParam
     g  <- liftIO (snapGame `fmap` readIORef ref)
     sendJSON $
       object [ "helpUrl" .=
                  fmap Text.pack
                     (featureHelpUrl =<< getFeatureAt a (land g)) ]

--------------------------------------------------------------------------------

newGame :: Act
newGame s = sendJSON =<< liftIO go
  where
  go = do r <- liftIO newStdGen
          let t = testGame r
          writeIORef s SnapState { snapGame = t, snapHistory = [] }
          return t

snapUpdateGameMaybe :: IORef SnapState -> (Game -> Maybe Game) -> Snap ()
snapUpdateGameMaybe ref f =
  do s1 <- liftIO $ atomicModifyIORef' ref $ \s ->
        let g = snapGame s
        in (\x -> (x,x)) $
           case f g of
             Just g1 -> SnapState { snapGame = g1
                                  , snapHistory = g : snapHistory s
                                  }
             Nothing -> s
     sendJSON (snapGame s1)


snapUpdateGame :: IORef SnapState -> (Game -> Game) -> Snap ()
snapUpdateGame ref f = snapUpdateGameMaybe ref (Just . f)

snapUpdatePlayer :: IORef SnapState -> (Player -> Player) -> Snap ()
snapUpdatePlayer ref f = snapMaybeUpdatePlayer ref (Just . f)

snapMaybeUpdatePlayer :: IORef SnapState -> (Player -> Maybe Player) -> Snap ()
snapMaybeUpdatePlayer ref f = snapUpdateGameMaybe ref $
                              \g -> doWriteLoc g thePlayer f





snapClickHex :: Act
snapClickHex s =
  do addr <- addrParam
     sendJSON $ object [ "tag"   .= ("menu" :: Text)
                       , "items" .= [ Text.pack ("move to " ++ show addr)
                                    , "info"
                                    ]
                       ]



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
snapUpdateOffers ref f = snapUpdateGame ref $ \g -> writeLoc g theOffers f

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
     snapUpdateGame ref (\g -> writeLoc g thePlayArea (powerUpCard i))


snapSpendMana :: Act
snapSpendMana ref =
  do c <- manaParam "color"
     snapUpdateGame ref (\g -> writeLoc g thePlayArea (removeManaToken c))


--------------------------------------------------------------------------------

snapMovePlayer :: Act
snapMovePlayer ref =
  do a <- addrParam
     snapUpdateGameMaybe ref $ \g ->
       isOk (if addrOnMap a g then movePlayerTo a g else explore a g)


