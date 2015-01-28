{-# LANGUAGE Trustworthy #-}
module MageKnight.JSON
  ( Export(..)
  , jsonBytes
  , (.=)
  , object
  , JS.Value
  ) where

import           Data.Text(Text)
import qualified Data.Aeson as JS
import qualified Data.Aeson.Types as JS
import qualified Data.Vector as Vector
import qualified Data.ByteString.Lazy as LBS

class Export a where
  toJS :: a -> JS.Value

(.=) :: Export a => Text -> a -> JS.Pair
x .= y = x JS..= toJS y

object :: [ JS.Pair ] -> JS.Value
object = JS.object

jsonBytes :: Export a => a -> LBS.ByteString
jsonBytes = JS.encode . toJS

instance Export Int where
  toJS n = JS.Number (fromIntegral n)

instance Export Text where
  toJS xs = JS.String xs

instance Export JS.Value where
  toJS x = x

instance Export a => Export (Maybe a) where
  toJS x = case x of
             Nothing -> JS.Null
             Just a  -> toJS a

instance Export a => Export [a] where
  toJS xs = JS.Array (Vector.fromList (map toJS xs))

