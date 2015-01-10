{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import MK
import MageKnight.Enemies

import           Snap.Http.Server (quickHttpServe)
import           Snap.Core (Snap)
import qualified Snap.Core as Snap
import           Snap.Util.FileServe(serveDirectory, serveFile)


import           Data.Char(toLower, isAlphaNum, isAscii)
import           Data.List(find)
import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text

import           Control.Applicative ((<|>))

import           System.FilePath((</>), (<.>))

main :: IO ()
main = quickHttpServe $ Snap.route
  [ ("/card/img/:name", sendCard)
  , ("/enemy/img/:name", sendEnemy)
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




