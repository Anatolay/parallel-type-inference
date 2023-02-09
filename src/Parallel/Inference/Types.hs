{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Parallel.Inference.Types where

import Data.Array.Accelerate as A

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
type VectorizedType = (Tag, Int)
type TypePair = (VectorizedType, VectorizedType)

type TypeMatrix = Vector (VectorizedType, VectorizedType, VectorizedType)
type ConstraintMatrix = Vector TypePair


