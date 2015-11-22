{-# LANGUAGE OverloadedStrings #-}
module Skill where

import Util.JSON

import Data.Text (Text)

-- XXX
data Skill = SkillXXX

instance Export Skill where
  toJS s =
    case s of
      SkillXXX -> toJS ("SkillXXX" :: Text)

