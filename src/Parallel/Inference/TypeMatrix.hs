{-# LANGUAGE TupleSections #-}
module Parallel.Inference.TypeMatrix where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Parallel.Inference.Types
import Parallel.Inference.Substitution
import Parallel.AST (TypeIndex)

initTypeMatrixByDepth :: Int -> TypeMatrix
initTypeMatrixByDepth depth = matrix
  where
    shape = Z :. depth
    types = A.fromList shape $ P.fmap (variableTag, ) [0..(depth-1)]
    -- args = A.fromList shape $ P.fmap (noneTag, ) [0..]
    args = A.fromList (Z :. (0 :: Int)) []
    res = args
    matrix = (types, args, res)

initTypeMatrixWith :: Acc (Vector TypeIndex) -> Acc TypeMatrix
initTypeMatrixWith typeIndices = matrix
  where
    shp = shape typeIndices

    types = map (\x -> lift (variable_, x)) typeIndices
    -- args = fill shp $ lift (noneTag, 0 :: Int)
    args = A.fromList (Z :. (0 :: Int)) []
    res = args

    matrix = lift (types, args, res)

-- | Given a vector of indices and vector of values, put these values
-- into provided indices of type matrix
updateTypes :: Acc TypeMatrix -> Acc (Vector Int) -> Acc (Vector EncodedType) -> Acc TypeMatrix
updateTypes matrix indices values = updatedMatrix
  where
    T3 types args res = matrix
    updatedTypes = substitute' indices values types
    updatedMatrix = T3 updatedTypes args res

updateArgRes :: Acc TypeMatrix -> Acc TypeMatrix
updateArgRes (T3 types arg res) = updatedTypeMatrix
  where
    lookup :: Exp EncodedType -> Exp EncodedType
    lookup encTy@(T2 ty val) =
      (ty == variable_) ? (types !! val, encTy)

    updatedTypeMatrix = T3 types (map lookup arg) (map lookup res)

updateTypeMatrix :: Acc TypeMatrix -> Acc (Vector Int) -> Acc (Vector EncodedType) -> Acc TypeMatrix
updateTypeMatrix matrix indices values = selfUpdate $ updateTypes matrix indices values

selfUpdate :: Acc TypeMatrix -> Acc TypeMatrix
selfUpdate (T3 types args res) = T3 newTypes newArgs newRes
  where
    update :: Exp EncodedType -> Exp EncodedType
    update encTy@(T2 tag ind) = tag == variable_ ? (types !! ind, encTy)

    newTypes = map update types
    newArgs = map update args
    newRes = map update res
