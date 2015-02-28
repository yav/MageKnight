{-# LANGUAGE Safe #-}
module MageKnight.Random
  ( StdGen
  , shuffle
  , rollDie
  , rollDice
  , split
  ) where

import MageKnight.Common
import System.Random ( StdGen, randomR, split )

shuffle :: StdGen -> [a] -> ([a], StdGen)
shuffle g0 xs0 = go g0 (length xs0) xs0
  where
  go g len xs | len < 2 = (xs, g)
  go g len xs = let (n,g1)    = randomR (0,len - 1) g
                    (as,b:bs) = splitAt n xs
                    (ys,g2)   = go g1 (len - 1) (as ++ bs)
                in (b : ys, g2)

rollDie :: StdGen -> (Mana, StdGen)
rollDie g = let (n,g1) = randomR (0,5) g
            in (anyMana !! n, g1)

rollDice :: Int -> StdGen -> ([Mana], StdGen)
rollDice n g
  | n <= 0    = ([], g)
  | otherwise = let (x,g1) = rollDie g
                    (xs,g2) = rollDice (n-1) g1
                in (x:xs,g2)


