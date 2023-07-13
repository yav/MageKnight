module Deed.Action.Rage where

import Deed.Action

--- XXX
rageBasic :: DeedDef
rageBasic s =
  do selectAttackTargets "Ranged attack"

ragePower :: DeedDef
ragePower _ = pure ()


