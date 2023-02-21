{-# LANGUAGE TupleSections #-}
module Parallel.Inference.TypeMatrix where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Parallel.Inference.Types
import Parallel.Inference.Substitution

initTypeMatrix :: Int -> TypeMatrix
initTypeMatrix depth = matrix
  where
    shape = Z :. depth
    types = A.fromList shape $ P.fmap (variableTag, ) [0..(depth-1)]
    args = A.fromList shape $ P.fmap (noneTag, ) [0..]
    res = args
    matrix = (types, args, res)

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
    lookup vecTy@(T2 ty val) =
      (ty == variable_) ? (types !! val, vecTy)
    
    updatedTypeMatrix = T3 types (map lookup arg) (map lookup res)

updateTypeMatrix :: Acc TypeMatrix -> Acc (Vector Int) -> Acc (Vector EncodedType) -> Acc TypeMatrix
updateTypeMatrix matrix indices values = updateArgRes $ updateTypes matrix indices values
