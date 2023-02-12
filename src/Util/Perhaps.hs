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
  m >>= k = case m of
              Ok a     -> k a
              Failed t -> Failed t

instance MonadFail Perhaps where
  fail    = Failed . Text.pack

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

-- | Apply a function to a lt of elements.  Errors, if any, are collected
-- in the second part of the result.
mapPerhaps :: (a -> Perhaps b) -> [a] -> ([b], [Text])
mapPerhaps f = foldr cons ([],[])
  where cons a (bs,errs) = case f a of
                             Failed err -> (bs, err : errs)
                             Ok b       -> (b : bs, errs)


