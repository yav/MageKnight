> {-# LANGUAGE RecordWildCards #-}
> {-# LANGUAGE OverloadedStrings #-}

> module MK where
>
> import MageKnight.Common
> import MageKnight.Enemies
> import MageKnight.Rule
> import MageKnight.Cards
> import MageKnight.Bag
>
> import           Control.Monad ( zipWithM_ )
> import           Data.Char ( toLower )
> import           Text.PrettyPrint ( Doc, text, int, vcat, (<+>), (<>), empty
>                                   , nest )
> import           Data.Text (Text)
> import qualified Data.Text as Text
> import           Text.Read ( readMaybe)
> import           System.IO( hFlush, stdout )
> import           System.Random ( StdGen, randomR, newStdGen )

> data MageKnight = MageKnight
>   { mkFame        :: Int
>   , mkReputation  :: Int      -- ^ Index on board, *NOT* same as influence
>   , mkArmor       :: Int
>   , mkUnits       :: [ Maybe Unit ]
>   , mkCrystals    :: Bag BasicMana
>   , mkCardLimit   :: Int
>   , mkDeck        :: [CardName]
>   , mkHand        :: [CardName]
>   , mkDiscard     :: [CardName]
>   }

> newMK :: [CardName] -> MageKnight
> newMK deck = MageKnight
>   { mkFame       = 0
>   , mkReputation = 0
>   , mkArmor      = 2
>   , mkUnits      = [ Nothing ]
>   , mkCrystals   = bagEmpty
>   , mkCardLimit  = 5
>   , mkDeck       = deck
>   , mkHand       = []
>   , mkDiscard    = []
>   }

> fillHand :: MageKnight -> MageKnight
> fillHand MageKnight { .. } = MageKnight { mkDeck = deck1, mkHand = hand1, .. }
>   where
>   have = length mkHand
>   need = mkCardLimit - have
>   (newCards,deck1) = splitAt need mkDeck
>   hand1 = newCards ++ mkHand


> influenceFromReputation :: Int -> Maybe Int
> influenceFromReputation n =
>   case compare n 0 of
>     EQ -> Just 0
>     LT -> fmap negate val
>     GT -> val
>   where
>   val = lookup (abs n - 1) (zip [ 0 .. ] [ 0, 1, 1, 2, 2, 3, 5 ])


> data Unit = Unit







Play
====


> pickRules :: Bag Resource -> [ (Text, Bag Resource) ]
> pickRules rs = [ (ruleName r, rs1) | r <- rules, Just rs1 <- [ useRule r rs ]]

> play :: Bag Resource -> IO ()
> play rs =
>   do print (ppResources rs)
>      let (names, opts) = unzip (pickRules rs)
>      putStrLn "Options:"
>      zipWithM_ showOpt [ 1 :: Int .. ] names
>      choose (zip [ 1 :: Int .. ] opts)
>   where
>   showOpt n r = putStrLn (show n ++ ". " ++ Text.unpack r)
>   choose []   = return ()
>   choose opts = do putStr "> "
>                    hFlush stdout
>                    cs <- getLine
>                    case (`lookup` opts) =<< readMaybe cs of
>                      Just r1 -> play r1
>                      Nothing -> choose opts

> start = bagFromList [ ACard "March", ACard "March"
>                     , ManaSource Gold
>                     , ManaDie, ManaSource (BasicMana Green) ]



Randomness
----------

> shuffle :: StdGen -> [a] -> ([a], StdGen)
> shuffle g0 xs0 = go g0 (length xs0) xs0
>   where
>   go g len xs | len < 2 = (xs, g)
>   go g len xs = let (n,g1)    = randomR (0,len - 1) g
>                     (as,b:bs) = splitAt n xs
>                     (ys,g2)   = go g1 (len - 1) (as ++ bs)
>                 in (b : ys, g2)

> rollDie :: StdGen -> (Mana, StdGen)
> rollDie g = let (n,g1) = randomR (0,5) g
>             in (anyMana !! n, g1)



