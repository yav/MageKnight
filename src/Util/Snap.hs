{-# LANGUAGE OverloadedStrings #-}
module Util.Snap where

import Util.JSON(Export,jsonBytes)

import           Snap.Core (Snap)
import qualified Snap.Core as Snap

import           Data.ByteString(ByteString)
import qualified Data.ByteString as BS
import           Data.Text (Text)
import           Data.Text.Encoding(decodeUtf8)
import           Data.Text.Read(decimal)
import qualified Data.Text as Text



sendJSON :: Export a => a -> Snap ()
sendJSON a =
  do Snap.modifyResponse (Snap.setHeader "content-type" "application/json")
     Snap.writeLBS (jsonBytes a)

--------------------------------------------------------------------------------
-- Error reporting

badInput :: ByteString -> Snap a
badInput msg =
  Snap.finishWith (Snap.setResponseStatus 400 msg Snap.emptyResponse)

notFound :: Snap a
notFound = Snap.finishWith (Snap.setResponseStatus 404 "Not Found"
                                                      Snap.emptyResponse)


--------------------------------------------------------------------------------
-- Parameters

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


