{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
module Sigym4.Null.Accelerate.Maybe (
-- * Types
  Nullable (..)
, Maybe (..)
-- * Basic functions
, isNothing
, isJust
, fromMaybe
, maybe
-- * 'Nullable'
-- ** Basic functions
, fromNullable
) where

import           Sigym4.Null
import           Sigym4.Null.Maybe              (Maybe(..), MaskType, Nullable)
import qualified Sigym4.Null.Maybe              as M

import           Control.Newtype
import           Data.Array.Accelerate                    as A
import           Data.Array.Accelerate.Smart
import           Data.Array.Accelerate.Product
import           Data.Array.Accelerate.Array.Sugar

import           Prelude as P hiding (Maybe(..), maybe)

isNothing
  :: forall a.
   ( HasNull a
   , Elt (Nullable a)
   , Unlift Exp (Nullable a)
   , Plain (Nullable a) ~ Nullable a
   ) => Exp (Nullable a) -> Exp Bool
isNothing = lift1 (M.isNothing :: Nullable a -> Bool)
{-# INLINE isNothing #-}

isJust
  :: forall a.
   ( HasNull a
   , Elt (Nullable a)
   , Unlift Exp (Nullable a)
   , Plain (Nullable a) ~ Nullable a
   ) => Exp (Nullable a) -> Exp Bool
isJust = lift1 (M.isJust :: Nullable a -> Bool)
{-# INLINE isJust #-}

fromMaybe
  :: forall a.
   ( HasNull a
   , Elt a
   , Elt (Nullable a)
   , Unlift Exp (Nullable a)
   , Unlift Exp a
   , Plain a ~ a
   , Plain (Nullable a) ~ Nullable a
   ) => Exp a -> Exp (Nullable a) -> Exp a
fromMaybe = lift2 (M.fromMaybe :: a -> Nullable a -> a)
{-# INLINE fromMaybe #-}

maybe
  :: forall a b.
   ( HasNull a
   , Elt a
   , Elt (Nullable a)
   , Elt b
   , Unlift Exp a
   , Unlift Exp (Nullable a)
   , Plain (Nullable a) ~ Nullable a
   , Plain a ~ a
   ) => Exp b -> (Exp a -> Exp b) -> Exp (Nullable a) -> Exp b
maybe b f m = isJust m A.? (f (fromMaybe undefined m), b)
{-# INLINE maybe #-}

fromNullable
  :: forall a.
   ( HasNull a
   , Elt (Nullable a)
   , Unlift Exp a
   , Lift Exp (Nullable a)
   , Plain (Nullable a) ~ Nullable a
   , Plain a ~ a
   ) => Exp a -> Exp (Nullable a)
fromNullable = lift1 (M.fromNullable :: a -> Nullable a)
{-# INLINE fromNullable #-}


type instance EltRepr (Maybe a) = EltRepr (EltRepr MaskType, a)

maskTrue, maskFalse :: MaskType
maskTrue = 1
maskFalse = 0

instance (Elt MaskType, Elt a) => Elt (Maybe a) where
  eltType _ = eltType (undefined :: (MaskType,a))
  toElt p = case toElt p of
     (x, _) | x P.== maskFalse -> Nothing
     (_, y)                    -> Just y
  fromElt Nothing  = fromElt (maskFalse, undefined::a)
  fromElt (Just y) = fromElt (maskTrue, y)

instance (cst a, cst MaskType) => IsProduct cst (Maybe a) where
  type ProdRepr (Maybe a) = ProdRepr (MaskType,a)
  fromProd p (Just y) = fromProd p (maskTrue, y)
  fromProd p Nothing  = fromProd p (maskFalse, undefined::a)
  toProd p t = case toProd p t of
     (x, _) | x P.== maskFalse -> Nothing
     (_, y)                    -> Just y
  prod p _ = prod p (undefined :: (MaskType,a))

instance (Lift Exp a, Elt (Plain a), Plain a ~ a) => Lift Exp (Maybe a) where
  type Plain (Maybe a) = Maybe (Plain a)
  lift Nothing  = Exp $ Tuple $ NilTup `SnocTup` lift maskFalse `SnocTup` lift (undefined::a)
  lift (Just y) = Exp $ Tuple $ NilTup `SnocTup` lift maskFalse `SnocTup` lift y
