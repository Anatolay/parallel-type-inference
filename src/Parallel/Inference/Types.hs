{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
-- {-# LANGUAGE FlexibleContexts #-}

module Parallel.Inference.Types where

import qualified Prelude as P
import Data.Array.Accelerate as A

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

type ConstraintMatrix = Vector TypePair

-- | Remove all rows where two type are equal.
--
--  ___________
-- | Nat |  8  |
-- | f9  | f9  |
-- -------------
-- row with two f9's gets removed
--
removeRepeats :: Acc ConstraintMatrix -> Acc ConstraintMatrix
removeRepeats matrix = elems
  where
    (T2 elems _) = filter predicate matrix
    predicate (T2 ty0 ty1) =
      fst ty0 /= fst ty1 || snd ty0 /= snd ty1

expandFunctionEquality :: Acc ConstraintMatrix -> Acc (Vector TypePair) -> Acc ConstraintMatrix
expandFunctionEquality matrix funReference =
  let
    expandCount :: Exp TypePair -> Exp Int
    expandCount (T2 (T2 ty0 _) (T2 ty1 _))
      =  ty0 == function_
      && ty1 == function_
      ?  (2, 1)

    expandTarget :: Exp TypePair -> Exp Int -> Exp TypePair
    expandTarget pair i =
      let
        (T2 (T2 ty0 ind0) (T2 ty1 ind1)) = pair
        fun0 = funReference !! ind0
        fun1 = funReference !! ind1
        argTyPair = T2 (fst fun0) (fst fun1)
        resTyPair = T2 (snd fun0) (snd fun1)
        target = i == 1 ? (resTyPair, argTyPair)
      in
        ty0 == function_ && ty1 == function_ ? (target, pair)

  in expand expandCount expandTarget matrix

leftifyTypeVariables :: Acc ConstraintMatrix -> Acc ConstraintMatrix
leftifyTypeVariables = map leftify
  where
    leftify pair@(T2 vecTy0 vecTy1) =
      fst vecTy1 == variable_ ? (T2 vecTy1 vecTy0, pair)
