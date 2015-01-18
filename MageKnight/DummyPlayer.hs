{-# LANGUAGE RecordWildCards #-}
module MageKnight.DummyPlayer where


import MageKnight.Random
import MageKnight.Common
import MageKnight.Bag

data DummyPlayer = DummyPlayer
  { dummyPlayerCrystals :: Bag BasicMana
  , dummyPlayerDeck     :: [BasicMana]
  , dummyPlayerDiscards :: [BasicMana]
  , dummyPlayerRand     :: StdGen
  }


dummyPlayeNew :: StdGen -> [BasicMana] -> DummyPlayer
dummyPlayeNew r cs =
  DummyPlayer { dummyPlayerCrystals = bagFromList cs
              , dummyPlayerDeck     = deck
              , dummyPlayerDiscards = []
              , dummyPlayerRand     = r1
              }
  where
  (deck, r1) = shuffle r initalDeck
  initalDeck = concatMap (replicate 4) anyBasicMana

dummyPlayerTurn :: DummyPlayer -> Maybe DummyPlayer
dummyPlayerTurn DummyPlayer { .. } =
  case dummyPlayerDeck of
    [] -> Nothing
    ds -> let (as,bs)   = splitAt 3 ds
              a         = last as
              (as',bs') = splitAt (bagLookup a dummyPlayerCrystals) bs
          in Just DummyPlayer { dummyPlayerDeck = bs'
                              , dummyPlayerDiscards = as ++ as'
                              , ..
                              }

dummyPlayerNewRound :: DummyPlayer -> BasicMana -> BasicMana -> DummyPlayer
dummyPlayerNewRound DummyPlayer { .. } newCrystal newCard =
  DummyPlayer { dummyPlayerCrystals = bagAdd 1 newCrystal dummyPlayerCrystals
              , dummyPlayerDeck = newDeck
              , dummyPlayerDiscards = []
              , dummyPlayerRand = newR
              }
    where
    (newDeck,newR) = shuffle dummyPlayerRand (newCard : dummyPlayerDeck ++
                                                        dummyPlayerDiscards)


