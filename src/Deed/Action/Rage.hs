module Deed.Action.Rage where

import Common
import Deed.Action

--- XXX
rageBasic :: DeedDef
rageBasic s =
  do doAttack 2 Physical

ragePower :: DeedDef
ragePower _ = pure ()


