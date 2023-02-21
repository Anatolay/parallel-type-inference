module Parallel.Inference.Substitution where

import Parallel.Inference.Types
import Data.Array.Accelerate as A

substitute :: Acc SubstitutionMatrix -> Acc (Vector EncodedType) -> Acc (Vector EncodedType)
substitute sub vec = scatter indices vec vals
  where
    T2 indices vals = sub

substitute' :: Acc (Vector Int) -> Acc (Vector EncodedType) -> Acc (Vector EncodedType) -> Acc (Vector EncodedType)
substitute' indicies subVals = substitute (T2 indicies subVals)
