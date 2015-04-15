{-# LANGUAGE Trustworthy, OverloadedStrings #-}
module MageKnight.JSON
  ( Export(..)
  , jsonBytes
  , (.=)
  , object
  , jsNull
  , JS.Value
  , ExportAsKey(..)
  , jsKey
  ) where

import           Data.Text(Text)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS

class Export a where
  toJS :: a -> JS.Value

class ExportAsKey a where
  toKeyJS :: a -> Text

(.=) :: Export a => Text -> a -> JS.Pair
x .= y = x JS..= toJS y

object :: [ JS.Pair ] -> JS.Value
object = JS.object

jsNull :: JS.Value
jsNull = JS.Null

jsonBytes :: Export a => a -> LBS.ByteString
jsonBytes = JS.encode . toJS

jsKey :: ExportAsKey a => a -> JS.Value
jsKey = toJS . toKeyJS

instance Export Bool where
  toJS n = case n of
             True  -> toJS ("true" :: Text)
             False -> toJS ("false" :: Text)

instance Export Int where
  toJS n = JS.Number (fromIntegral n)

instance Export Text where
  toJS xs = JS.String xs

instance ExportAsKey Text where
  toKeyJS = id

instance Export JS.Value where
  toJS x = x

instance Export a => Export (Maybe a) where
  toJS x = case x of
             Nothing -> JS.Null
             Just a  -> toJS a

instance Export a => Export [a] where
  toJS xs = JS.Array (Vector.fromList (map toJS xs))

