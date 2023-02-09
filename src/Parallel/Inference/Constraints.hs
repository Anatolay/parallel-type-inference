-- {-# LANGUAGE FlexibleContexts #-}

module Parallel.Inference.Constraints where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Parallel.Inference.Types

-- | Remove all rows where two types are equal.
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

-- | For each row with two function types
-- expand into two rows: argument type equality
-- and return type equality
--
-- a -> b == c -> d   =>   a == c && b == d
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

-- | For each type variable on the right, swap it with
-- the value on the left
leftifyTypeVariables :: Acc ConstraintMatrix -> Acc ConstraintMatrix
leftifyTypeVariables = map leftify
  where
    leftify pair@(T2 vecTy0 vecTy1) =
      fst vecTy1 == variable_ ? (T2 vecTy1 vecTy0, pair)

-- | Substitute all occurencies of a type variable in Constraint Matrix
-- with a value from typeMap
--
-- In typeMap, each value on the left is a type variable and
-- each value on the right is not a type variable (not enforced, unfortunately)
substituteTypeVariable :: Acc ConstraintMatrix -> Acc ConstraintMatrix -> Acc ConstraintMatrix
substituteTypeVariable typeMap matrix =
  let
    -- If type variable is in map, return the stored value
    -- Otherwise, return the type variable itself
    lookup :: Exp VectorizedType -> Acc ConstraintMatrix -> Exp VectorizedType
    lookup vecTy@(T2 _ val) typeMap =
      let
        typeValues = afst $ filter (\(T2 (T2 _ key) _) -> key == val) typeMap
      in null typeValues ? (vecTy, snd $ typeValues !! 0)

    substitute (T2 vecTy0 vecTy1) =
      T2 (lookup vecTy0 typeMap) (lookup vecTy1 typeMap)

  in map substitute matrix

-- | Return True if there is an error
-- e.g. equality of a function type and a base type
checkErrors :: Acc ConstraintMatrix -> Acc (Scalar Bool)
checkErrors = any isError
  where
    isError (T2 (T2 ty0 _) (T2 ty1 _))
      =  ty0 == nat_ && ty1 == function_
      || ty0 == function_ && ty1 == nat_

