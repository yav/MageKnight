{-# LANGUAGE OverloadedStrings #-}
module Server where

import Common(Mana(..), BasicMana(..))
import Terrain(Addr(..),Dir(..),HexAddr(..),hexAddr)
import DeedDecks(Deed,findDeed)
import Units(Unit,findUnit)
import Paths(deedPath,unitPath,resourceDir)
import Util.Snap
-- import Util.History(history)
-- import Util.JSON

import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
-- import qualified Data.Text as Text
-- import           Data.List(nub)
-- import           Data.IORef
import           Control.Applicative((<|>))
import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap, route)
import           Snap.Util.FileServe(serveDirectory, serveFile)


startServer :: IO ()
startServer =
  do quickHttpServe (route commands <|> serveDirectory resourceDir)
  where
  commands =
    [ ("/deed/:deed", getDeedImage)
    , ("/unit/:unit", getUnitImage)
    ]



-- | Get the image for a deed.
getDeedImage :: Snap ()
getDeedImage =
  do deed <- deedParam "deed"
     serveFile (deedPath deed)


-- | Get the image for a unit.
getUnitImage :: Snap ()
getUnitImage =
  do unit <- unitParam "unit"
     serveFile (unitPath unit)


{-
snapGetMapHelpUrl :: Act
snapGetMapHelpUrl ref =
  do a  <- addrParam
     g  <- liftIO (snapGame `fmap` readIORef ref)
     sendJSON $
       object [ "helpUrl" .=
                  fmap Text.pack
                     (getBuildingHelpUrls =<<
                       maybeToList (getFeatureAt a (land g)))
              , "enemyPowers" .=
                      nub (concatMap (map Text.pack . enemyPowerHelpUrl)
                               (getRevealedEnemiesAt a (land g)))
              , "offerUrls" .=
                  nub ( fmap (Text.pack . unitUrl) (unitsForHire a g) ++
                        fmap (Text.pack . deedUrl) (availableDeeds a g) )
              ]

-}


--------------------------------------------------------------------------------
-- Parameters for game-related types.

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



