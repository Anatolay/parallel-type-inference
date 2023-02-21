{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Parallel.Inference.Types where

import Data.Array.Accelerate as A


-- WARNING!! Handle with caution
noneTag :: Int
noneTag = -1
variableTag :: Int
variableTag = 0
functionTag :: Int
functionTag = 1
natTag :: Int
natTag = 2

none_ :: Exp Int
none_ = -1
variable_ :: Exp Int
variable_ = 0
function_ :: Exp Int
function_ = 1
nat_ :: Exp Int
nat_ = 2

data Type
  = Variable Int
  | Function Int
  | Nat
  deriving (Generic, Elt)

type Tag = Int
type EncodedType = (Tag, Int)
type TypePair = (EncodedType, EncodedType)

type TypeMatrix = (Vector EncodedType, Vector EncodedType, Vector EncodedType)
type ConstraintMatrix = Vector TypePair
type SubstitutionMatrix = (Vector Int, Vector EncodedType)

