module Deed.Action
  ( module Deed.Action
  , module X
  ) where

import Data.Text(Text)
import Data.Map(Map)
import Data.Map qualified as Map

import KOI.Field                    as X

import Common                       as X
import AppTypes                     as X
import State                        as X
import Input                        as X

data DeedAction = DeedAction
  { actBasic :: Interact ()
  , actPower :: Interact ()
  }

type Deeds = Map Text DeedAction
type DeedDef = State -> Interact ()

defDeeds :: [(Text,DeedAction)] -> Deeds
defDeeds = Map.fromList

unionDeeds :: [Deeds] -> Deeds
unionDeeds = Map.unions

defDeed :: Text -> DeedDef -> DeedDef -> (Text, DeedAction)
defDeed k a b = (k, DeedAction { actBasic = mk a, actPower = mk b })
  where
  mk f = getState >>= f

getAction :: Text -> Deeds -> DeedAction
getAction nm = Map.findWithDefault (deedNotImplemented nm) nm

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

