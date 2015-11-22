{-# LANGUAGE Safe #-}
module Util.Random
  ( Gen
  , genRand, genRandFun
  , StdGen, randSource, randSourceIO

  , shuffle
  , oneOf
  , randStdGen
  ) where

import qualified System.Random as Rand
import Data.Array (listArray, bounds, (!))
import Control.Monad(ap,liftM)

newtype StdGen = StdGen Rand.StdGen

randSourceIO :: IO StdGen
randSourceIO = StdGen <$> Rand.newStdGen

randSource :: Int -> StdGen
randSource = StdGen . Rand.mkStdGen

genRand :: StdGen -> Gen a -> (a, StdGen)
genRand g m = genRandFun g $ do a <- m
                                return $ \r -> (a,r)

genRandFun :: StdGen -> Gen (StdGen -> a) -> a
genRandFun (StdGen g) (Gen m) = let (f,g1) = m g
                                in f (StdGen g1)


newtype Gen a = Gen (Rand.StdGen -> (a,Rand.StdGen))

instance Functor Gen where
  fmap = liftM

instance Applicative Gen where
  pure a = Gen (\g -> (a,g))
  (<*>)  = ap

instance Monad Gen where
  Gen m >>= k = Gen (\g -> let (a,g1) = m g
                               Gen m1 = k a
                           in m1 g1)


randStdGen :: Gen StdGen
randStdGen = Gen (\g -> let (g1,g2) = Rand.split g
                        in (StdGen g1, g2))

shuffle :: [a] -> Gen [a]
shuffle xs0 = Gen (\g0 -> go g0 (length xs0) xs0)
  where
  go g len xs | len < 2 = (xs, g)
  go g len xs = let (n,g1)    = Rand.randomR (0,len - 1) g
                    (as,b:bs) = splitAt n xs
                    (ys,g2)   = go g1 (len - 1) (as ++ bs)
                in (b : ys, g2)

oneOf :: [a] -> Gen a
oneOf xs = Gen (\g -> let (n,g1) = Rand.randomR (bounds as) g
                      in (as ! n, g1))
  where
  as = listArray (0, length xs - 1) xs



