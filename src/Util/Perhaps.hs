{-# LANGUAGE Safe #-}
module Util.Perhaps where

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Control.Applicative as A
import           Control.Monad(liftM,ap)

data Perhaps a = Failed Text
               | Ok a

instance Functor Perhaps where
  fmap = liftM

instance A.Applicative Perhaps where
  pure  = return
  (<*>) = ap

instance Monad Perhaps where
  return  = Ok
  fail    = Failed . Text.pack
  m >>= k = case m of
              Ok a     -> k a
              Failed t -> Failed t

checkThat :: Bool -> Text -> Perhaps ()
checkThat b t = if b then Ok () else Failed t

perhaps :: Text -> Maybe a -> Perhaps a
perhaps t mb = case mb of
                 Nothing -> Failed t
                 Just a  -> Ok a

isOk :: Perhaps a -> Maybe a
isOk a = case a of
           Ok a'    -> Just a'
           Failed _ -> Nothing

