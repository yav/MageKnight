module DummyPlayer
  ( DummyPlayer
  , dummyPlayerNew
  , dummyPlayerTurn
  , dummyPlayerNewRound
  ) where

import Common

import Util.Bag
import Util.Random

data DummyPlayer = DummyPlayer
  { dummyPlayerCrystals :: Bag BasicMana
  , dummyPlayerDeck     :: [BasicMana]
  , dummyPlayerDiscards :: [BasicMana]
  , dummyPlayerRand     :: StdGen
  }


dummyPlayerNew :: [BasicMana] -> Gen DummyPlayer
dummyPlayerNew cs =
  do deck <- shuffle initalDeck
     r1   <- randStdGen
     return DummyPlayer { dummyPlayerCrystals = bagFromList cs
                        , dummyPlayerDeck     = deck
                        , dummyPlayerDiscards = []
                        , dummyPlayerRand     = r1
                        }
  where
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
  genRandFun dummyPlayerRand $
    do newDeck <- shuffle (newCard : dummyPlayerDeck ++ dummyPlayerDiscards)
       return $ \newR ->
          DummyPlayer
            { dummyPlayerCrystals = bagAdd 1 newCrystal dummyPlayerCrystals
            , dummyPlayerDeck     = newDeck
            , dummyPlayerDiscards = []
            , dummyPlayerRand     = newR
            }

