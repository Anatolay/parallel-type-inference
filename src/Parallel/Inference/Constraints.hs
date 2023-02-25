-- {-# LANGUAGE FlexibleContexts #-}

module Parallel.Inference.Constraints where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Parallel.Inference.Types

-- | Returns False if all values in the constraint matrix are type variables.
-- In this case, constraints can no longer be resolved.
hasNonTypeVars :: Acc ConstraintMatrix -> Acc (Scalar Bool)
hasNonTypeVars = any isNonVar
  where
    -- check if any in the pair is nonVar
    isNonVar :: Exp TypePair -> Exp Bool
    isNonVar (T2 (T2 tag0 _) (T2 tag1 _)) = tag0 /= variable_ || tag1 /= variable_

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
    -- If function equality, allocate two rows for new equalities,
    -- else leave single row as is
    expandCount :: Exp TypePair -> Exp Int
    expandCount (T2 (T2 ty0 _) (T2 ty1 _))
      =  ty0 == function_
      && ty1 == function_
      ?  (2, 1)

    -- If function equality, one row is for argument type equality (i is 0),
    -- the other is for result type equality (i is 1).
    -- Else keep row as is
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
      (fst vecTy1 == variable_) ? (T2 vecTy1 vecTy0, pair)

-- | Extract constraints of the form `type var=other` and generate a mapping
generateTypeMap :: Acc ConstraintMatrix -> (Acc (Vector Int), Acc (Vector EncodedType))
generateTypeMap matrix =
  let
    T2 typeMap _ = filter predicate matrix

    predicate (T2 (T2 ty0 _) (T2 ty1 _)) =
      ty0 == variable_ && ty1 /= variable_
    
    indices = map (\(T2 (T2 _ ind) _) -> ind) typeMap
    values = map (\(T2 _ val) -> val) typeMap
  in (indices, values)

-- TODO: use values from type matrix which should be more efficient
-- | Substitute all occurencies of a type variable in Constraint Matrix
-- with a value from type vector
--
-- For each type variable n in the constraint matrix, it is substituted
-- with the type at index n of the type vector
substituteTypeVariables :: Acc (Vector EncodedType) -> Acc ConstraintMatrix -> Acc ConstraintMatrix
substituteTypeVariables typeVec matrix =
  let
    lookup :: Exp EncodedType -> Exp EncodedType
    lookup vecTy@(T2 ty val) =
      (ty == variable_) ? (typeVec !! val, vecTy)
    
    substitute :: Exp TypePair -> Exp TypePair
    substitute (T2 vecTy0 vecTy1) = T2 (lookup vecTy0) (lookup vecTy1)

  in map substitute matrix

updateTypeVariables :: Acc ConstraintMatrix -> Acc TypeMatrix -> Acc ConstraintMatrix
updateTypeVariables constraintMatrix typeMatrix = updatedConstraintMatrix
  where
    (col0, col1) = unzip constraintMatrix
    T3 types _ _ = typeMatrix
    lookup :: Exp EncodedType -> Exp EncodedType
    lookup encTy@(T2 tag ind) =
      (tag == variable_) ? (types !! ind, encTy)
    updatedConstraintMatrix = zip (map lookup col0) (map lookup col1)


-- | Return True if there is an error
-- e.g. equality of a function type and a base type
checkErrors :: Acc ConstraintMatrix -> Acc (Scalar Bool)
checkErrors = any isError
  where
    isError (T2 (T2 ty0 _) (T2 ty1 _))
      =  ty0 == nat_ && ty1 == function_
      || ty0 == function_ && ty1 == nat_

-- Returns True is there is no error
checkNoErrors :: Acc ConstraintMatrix -> Acc (Scalar Bool)
checkNoErrors cm = map not (checkErrors cm)
