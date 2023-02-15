{-# LANGUAGE TupleSections #-}
module Parallel.Inference.TypeMatrix where

import qualified Prelude as P
import Data.Array.Accelerate as A
import Parallel.Inference.Types

initTypeMatrix :: Int -> TypeMatrix
initTypeMatrix depth = matrix
  where
    shape = Z :. depth
    types = A.fromList shape $ P.fmap (variableTag, ) [0..(depth-1)]
    args = A.fromList shape $ P.fmap (noneTag, ) [0..]
    res = args
    matrix = (types, args, res)

updateTypes :: Acc TypeMatrix -> Acc (Vector Int) -> Acc (Vector VectorizedType) -> Acc TypeMatrix
updateTypes matrix indices values = updatedMatrix
  where
    T3 types args res = matrix
    updatedTypes = scatter indices types values
    updatedMatrix = T3 updatedTypes args res

