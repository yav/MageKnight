{-# LANGUAGE Rank2Types #-}
module MageKnight.Loc where

type MonoLoc x a = Loc x x a a

newtype Loc x y a b =
  Loc (forall f. Functor f => (a -> f b) -> (x -> f y))

loc :: (x -> a) -> (x -> b -> y) -> Loc x y a b
loc get set = Loc upd
  where upd f y = fmap (set y) (f (get y))


readLoc :: x -> Loc x y a b -> a
readLoc x (Loc f) = y
  where K y = f K x

writeLoc :: x -> Loc x y a b -> (a -> b) -> y
writeLoc x l u = y
  where I y = doWriteLoc x l (I . u)

doWriteLoc :: Functor f => x -> Loc x y a b -> (a -> f b) -> f y
doWriteLoc x (Loc f) u = f u x


(~>) :: Loc x y p q -> Loc p q a b -> Loc x y a b
Loc f ~> Loc g = Loc (f . g)


--------------------------------------------------------------------------------
newtype I a   = I a
newtype K x a = K x

instance Functor I where
  fmap f (I a) = I (f a)

instance Functor (K x) where
  fmap _ (K a)  = K a


