import Data.Array.Accelerate as A
import Data.Array.Accelerate.LLVM.Native (run)
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.Inference.Algo (resolveConstraints)

var x = (variableTag, x)
nat = (natTag, 0)
fn x = (functionTag, x)
none = (noneTag, 0)

constraintMatrix :: Acc ConstraintMatrix
constraintMatrix = A.use $ A.fromList (Z :. 9)
  [ (nat, var 8)
  , (fn 12, fn 5)
  , (var 4, fn 7)
  , (fn 9, fn 9) -- repeat
  , (var 2, var 3)
  , (var 2, nat)
  , (var 0, var 0) -- repeat
  , (var 7, nat)
  , (nat, nat) -- repeat
  ]

typeMatrix :: Acc TypeMatrix
typeMatrix = A.use (vec, arg, res)
  where
    vec = A.fromList (Z :. 15) $ fmap var [0..]
    arg = A.fromList (Z :. 15) $ repeat none
    res = arg

execute = print . A.toList . run

main :: IO ()
main = do
  print . A.toList . run $ constraintMatrix
  print ""
  print . A.toList . run $ removeRepeats constraintMatrix
  print ""
  let (updTypeMatrix, updConstraintMatrix) = resolveConstraints typeMatrix constraintMatrix
  -- execute updTypeMatrix
  -- print ""
  execute updConstraintMatrix
  print ""
  let T3 types _ _ = typeMatrix
  let T3 newTypes _ _ = updTypeMatrix
  print . A.toList . run $ types
  print . A.toList . run $ newTypes

