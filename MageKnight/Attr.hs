{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE ConstraintKinds #-}
module MageKnight.Attr
  ( Attr, Obj, Val, Obj', Val'
  , Mono, IsMono
  , Poly, IsPoly
  , attr
  , (~>)
  , readAttr, writeAttr, writeAttr'
  ) where

-- | An attribute associates an object and a value.
-- Changes to the object affect the value, and vice versa:
-- changes to the value affect the object.
newtype Attr a =
  Attr (forall f. Functor f => (Val a -> f (Val' a)) ->
                               (Obj a -> f (Obj' a)))

{- | Describes attributes where values may be changed only to values
of the same type.

Note: This should be used only when constructing attributes values.
For parameter attributes, use the 'IsMono' constraint instead.  -}
type Mono x a       = Poly x x a a

{- | A convenient abbreviation, that makes it easy to constraint
the object and value for attribute parameters that not change the type of
values when updating.  -}
type IsMono p x a   = IsPoly p x x a a


-- | Describes attributes where values may be updated to values of
-- of a potentially different type.
type Poly x y a b = (x,y,a,b)

{- | A convenient abbreviation, that makes it easy to constraint
the objects and values for attribute parameters that change the type of
values when updating.  -}
type IsPoly p x y a b = (Obj p ~ x, Val p ~ a, Obj' p ~ y, Val' p ~ b)

-- | The type of the object associated with an attribute.
type family Obj a where
  Obj (x,y,a,b)   = x

-- | The type of the object associated with an attribute, after an update.
type family Obj' a where
  Obj' (x,y,a,b)  = y

-- | The type of the value of an attribute.
type family Val a where
  Val (x,y,a,b)   = a

-- | The type of the value of an attribute, after an update.
type family Val' a where
  Val' (x,y,a,b)  = b

-- | Define a new attribute.
attr ::
  (Obj a -> Val a)
  {- ^ How to compute the value, given an object -} ->
  (Obj a -> Val' a -> Obj' a)
  {- ^ How to compute a new object, given the old object and a new value -} ->
  Attr a
attr get set = Attr (\f y -> fmap (set y) (f (get y)))

-- | Get the value associated with an object.
readAttr :: Attr a -> Obj a -> Val a
readAttr (Attr f) x = y
  where K y = f K x

-- | Modify the value associated with an object.
writeAttr :: Attr a -> (Val a -> Val' a) -> Obj a -> Obj' a
writeAttr l u x = y
  where I y = writeAttr' l (I . u) x

-- | Modify the value associated with an object, in the context of a functor.
writeAttr' :: Functor f => Attr a -> (Val a -> f (Val' a)) ->
                                      Obj a -> f (Obj' a)
writeAttr' (Attr f) u x = f u x

{- | Compose two attributes.
If the first attrobute associates @A@ with @P@, and the second attributes
associates @P@ with @X@, then the resulting attribute will associate
@A@ with @X@. -}
(~>) ::
  ( IsPoly f a a' p p'
  , IsPoly g p p' x x'
  ) =>
  Attr f -> Attr g -> Attr (Poly a a' x x')
Attr f ~> Attr g = Attr (f . g)



--------------------------------------------------------------------------------
newtype I a   = I a
newtype K x a = K x

instance Functor I where
  fmap f (I a) = I (f a)

instance Functor (K x) where
  fmap _ (K a)  = K a


