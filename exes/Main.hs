{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MageKnight.Enemies
import MageKnight.Cards
import MageKnight.Terrain
import MageKnight.JSON
import MageKnight.Game
import MageKnight.Player

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
import           Data.IORef ( IORef, newIORef, writeIORef
                            , atomicModifyIORef' )

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class(liftIO)

import           System.FilePath((</>), (<.>))
import           System.Random (newStdGen)


main :: IO ()
main =
  do r <- newStdGen
     s <- newIORef (testGame r)
     quickHttpServe $ Snap.route
       [ ("/card/img/:name", sendCard)
       , ("/enemy/img/:name", sendEnemy)

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


sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)


--------------------------------------------------------------------------------

cardImagePath :: Card -> FilePath
cardImagePath Card { .. } =
  "ui" </> "img" </> "cards" </> "deck" </> color </> name <.> "jpg"
  where
  color = nameToPath $ Text.pack $ show cardColor
  name  = nameToPath cardName


enemyImagePath :: Enemy -> FilePath
enemyImagePath Enemy { .. } =
  "ui" </> "img" </> "enemies" </> loc </> name <.> "png"
  where
  loc  = nameToPath $ Text.pack $ show enemyType
  name = nameToPath enemyName


nameToPath :: Text -> String
nameToPath = Text.unpack . Text.map cvt
  where
  cvt c | isAscii c && isAlphaNum c = toLower c
  cvt _                             = '_'

--------------------------------------------------------------------------------

sendCard :: Snap ()
sendCard =
  do name <- textParam "name"
     case find ((name ==) . cardName) cards of
       Just card -> serveFile (cardImagePath card)
       Nothing   -> notFound

sendEnemy :: Snap ()
sendEnemy =
  do name <- textParam "name"
     case find ((name ==) . enemyName) allEnemies of
       Just enemy -> serveFile (enemyImagePath enemy)
       Nothing    -> notFound

--------------------------------------------------------------------------------

newGame :: IORef Game -> Snap ()
newGame s = sendJSON =<< liftIO go
  where
  go = do r <- liftIO newStdGen
          let g = testGame r
          writeIORef s g
          return g

clickHex :: IORef Game -> Snap ()
clickHex s =
  do a <- addrParam
     g <- liftIO (atomicModifyIORef' s (go a))
     sendJSON g
  where
  go a g
    | Just d <- find ((a ==) . neighbour loc) allDirections =
      let g1 = if addrOnMap a g then movePlayer d g
               else fromMaybe g (explore loc d g)
      in (g1,g1)

    | otherwise = (g,g)

      where loc = playerLocation p
            p = player g



