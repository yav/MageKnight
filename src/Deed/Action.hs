module Deed.Action
  ( module Deed.Action
  , module X
  ) where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map

import KOI.Field                    as X
import KOI.Enum

import Common                       as X
import Game.KOI                     as X
import Game.State                   as X
import Game.Input                   as X

import Deed.Type

data DeedAction = DeedAction
  { actBasic :: Interact ()
  , actPower :: Interact ()
  }

type Deeds = Map DeedName DeedAction
type DeedDef = State -> Interact ()

defDeeds :: [(DeedName,DeedAction)] -> Deeds
defDeeds = Map.fromList

unionDeeds :: [Deeds] -> Deeds
unionDeeds = Map.unions

defDeed :: DeedName -> DeedDef -> DeedDef -> (DeedName, DeedAction)
defDeed k a b = (k, DeedAction { actBasic = mk a, actPower = mk b })
  where
  mk f = getState >>= f

getAction :: DeedName -> Deeds -> DeedAction
getAction nm =
  Map.findWithDefault (deedNotImplemented (enumToText nm)) nm

deedNotImplemented :: Text -> DeedAction
deedNotImplemented txt = DeedAction
  { actBasic = notImplemented txt
  , actPower = notImplemented txt
  }

 -- XXX: say something, when we have logging
notImplemented :: Text -> Interact ()
notImplemented _ = pure ()

gainMove :: Int -> Interact ()
gainMove n =
  do updateThe_ movement (n +)


