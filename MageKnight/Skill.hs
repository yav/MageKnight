{-# LANGUAGE OverloadedStrings #-}
module MageKnight.Skill where

import MageKnight.JSON

import Data.Text (Text)

-- XXX
data Skill = SkillXXX

instance Export Skill where
  toJS s =
    case s of
      SkillXXX -> toJS ("SkillXXX" :: Text)

