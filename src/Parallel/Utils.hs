module Parallel.Utils where

import Data.Array.Accelerate
import qualified Prelude as P

combine :: (Elt a, Elt b) => Acc (Vector a) -> Acc (Vector b) -> Acc (Vector (a, b))
combine = zip
