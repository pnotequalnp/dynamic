{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Module      : Dynamic
-- Description : Dynamic Typing
-- Copyright   : Kevin Mullins 2022
-- License     : ISC
-- Maintainer  : kevin@pnotequalnp.com
module Dynamic (
  Dynamic (Dynamic, MkDynamic),
  ($@),
  (.@),
  fromDynamic,
  fromDynamicOr,
  dynamicRep,
  IsDynamic (..),
  DynamicTypeError (..),
) where

import Control.Monad.Catch (Exception, MonadThrow (..))
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Type.Reflection (TypeRep, Typeable, eqTypeRep, typeRep, typeRepKind, pattern Fun, type (:~~:) (..), SomeTypeRep (SomeTypeRep))

data Dynamic :: Type where
  MkDynamic :: TypeRep a -> a -> Dynamic

pattern Dynamic :: forall a. Typeable a => a -> Dynamic
pattern Dynamic x <- (\case MkDynamic t x
                              | Just HRefl <- t `eqTypeRep` typeRep @a -> Just x
                              | otherwise -> Nothing
                     -> Just x)
  where Dynamic x = MkDynamic typeRep x

instance Show Dynamic where
  showsPrec _ (MkDynamic t _) = showString "<<" . shows t . showString ">>"

class IsDynamic a where
  toDynamic :: a -> Dynamic

instance IsDynamic Dynamic where
  toDynamic = id

instance {-# OVERLAPPABLE #-} Typeable a => IsDynamic a where
  toDynamic = Dynamic

data DynamicTypeError :: Type where
  DynamicApplication :: TypeRep a -> TypeRep b -> DynamicTypeError
  DynamicExtraction :: TypeRep a -> TypeRep b -> DynamicTypeError
  DynamicComposition :: TypeRep a -> TypeRep b -> DynamicTypeError

deriving stock instance Show DynamicTypeError

instance Exception DynamicTypeError

($@) :: (IsDynamic a, IsDynamic b, MonadThrow m) => a -> b -> m Dynamic
(toDynamic -> MkDynamic tf f) $@ (toDynamic -> MkDynamic tx x)
  | Fun a b <- tf
  , Just HRefl <- tx `eqTypeRep` a
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind b
  = pure (MkDynamic b (f x))
  | otherwise = throwM (DynamicApplication tf tx)

(.@) :: (IsDynamic a, IsDynamic b, MonadThrow m) => a -> b -> m Dynamic
(toDynamic -> MkDynamic tf f) .@ (toDynamic -> MkDynamic tg g)
  | Fun b' c <- tf
  , Fun a b <- tg
  , Just HRefl <- b `eqTypeRep` b'
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind a
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind b
  , Just HRefl <- typeRep @Type `eqTypeRep` typeRepKind c
  = pure (MkDynamic (Fun a c) (f . g))
  | otherwise = throwM (DynamicComposition tf tg)

fromDynamic :: forall a m. (MonadThrow m, Typeable a) => Dynamic -> m a
fromDynamic (MkDynamic t x)
  | Just HRefl <- t `eqTypeRep` typeRep @a = pure x
  | otherwise = throwM (DynamicExtraction t (typeRep @a))

fromDynamicOr :: Typeable a => a -> Dynamic -> a
fromDynamicOr x = fromMaybe x . fromDynamic

dynamicRep :: Dynamic -> SomeTypeRep
dynamicRep (MkDynamic t _) = SomeTypeRep t
