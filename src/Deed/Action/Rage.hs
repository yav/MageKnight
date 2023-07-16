module Deed.Action.Rage where

import Deed.Action

--- XXX
rageBasic :: DeedDef
rageBasic s =
  do selectAttackTargets

ragePower :: DeedDef
ragePower _ = pure ()


