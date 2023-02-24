{-# LANGUAGE TypeOperators #-}
module Parallel.Inference.Algo where

import Data.Array.Accelerate as A
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.AST

import qualified Prelude as P

runInference :: VectorTree -> Vector TypeIndex -> VectorTree
runInference = undefined

-- | Initialize type matrix for given tabular AST
bindVars :: Acc VectorTree -> Acc (Vector TypeIndex) -> (Acc VectorTree, Acc TypeMatrix)
bindVars vectorTree typeIndices = (vectorTree, typeMatrix)
  where
    typeMatrix = initTypeMatrixWith typeIndices

-- | Infer types of let bindings, collect constraints
letBindings :: Acc VectorTree -> Acc TypeMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
letBindings vectorTree typeMatrix = (vectorTree, typeMatrix, constraints)
  where
    T3 types _ _ = typeMatrix
    letNodeIndices = indicesByNodeType LetExpr vectorTree
    -- childIndices = childrenIndicesByParent LetExpr vectorTree

    -- -- For each Let node, there are 3 child nodes
    -- childGroups :: Acc (Matrix Int)
    -- childGroups = reshape (lift $ Z :. length letNodeIndices :. (3 :: Int)) childIndices

    -- vars = slice childGroups (constant (Z :. All :. (0 :: Int)))
    -- bound = slice childGroups (constant (Z :. All :. (1 :: Int)))
    -- body = slice childGroups (constant (Z :. All :. (2 :: Int)))

    vars = nthChildIndices LetExpr 0 vectorTree
    bound = nthChildIndices LetExpr 1 vectorTree
    body = nthChildIndices LetExpr 2 vectorTree
    
    letNodeTypes = map (types !!) letNodeIndices
    varTypes = map (types !!) vars
    boundTypes = map (types !!) bound
    bodyTypes = map (types !!) body

    -- Let expression has the same type as its body
    -- Introduced variable has the same type as the expression
    constraints :: Acc ConstraintMatrix
    constraints = zip letNodeTypes bodyTypes ++ zip varTypes boundTypes

-- | Infer types of known constants
inferConstants :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
inferConstants vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, newConstraintMatrix)
  where
    natNodes = indicesByNodeType Constant vectorTree
    T3 types _ _ = typeMatrix

    -- Here we use the fact that at this step in the algorithm,
    -- type matrix only contains type variables, which allows us
    -- to update all of them directly. In a more general case,
    -- this step should only generate more constraints
    vars = map (types !!) natNodes
    (_, indices) = unzip vars
    newTypeMatrix = updateTypeMatrix typeMatrix indices $ fill (shape types) (lift (nat_, 0 :: Int))
    newConstraintMatrix = updateTypeVariables constraintMatrix newTypeMatrix

-- | Infer types of succ and pred expressions
succTyping :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
succTyping vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, finalConstraintMatrix)
  where
    T3 types _ _ = typeMatrix

    succNodes = indicesByNodeType SuccOperator vectorTree
    predNodes = indicesByNodeType PredOperator vectorTree
    nodes = succNodes ++ predNodes

    -- succChildren = childrenIndicesByParent SuccOperator vectorTree
    -- predChildren = childrenIndicesByParent PredOperator vectorTree
    succChildren = nthChildIndices SuccOperator 0 vectorTree
    predChildren = nthChildIndices PredOperator 0 vectorTree
    childNodes = succChildren ++ predChildren

    -- We know that types for all Succ and Pred nodes should not be initialized,
    -- Therefore we can update them directly.
    vars = map (types !!) nodes
    (_, indices) = unzip vars
    newTypeMatrix = updateTypeMatrix typeMatrix indices $ fill (shape types) (lift (nat_, 0 :: Int))
    newConstraintMatrix = updateTypeVariables constraintMatrix newTypeMatrix

    -- Since some of the nat values could have been placed already, it is simpler for now
    -- to generate constraints instead of adding additional checks.
    childTypes = map (types !!) childNodes
    newConstraints = map (\ct -> lift (ct, lift (nat_, 0 :: Int) :: Exp EncodedType)) childTypes
    finalConstraintMatrix = newConstraintMatrix ++ newConstraints

-- | Generate constraints for lambda abstractions. Initialize function type matrix.
lambdaTyping :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
lambdaTyping vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, newConstraintMatrix)
  where
    T3 types args res = typeMatrix
    lambdaNodeIndices = indicesByNodeType LambdaAbstraction vectorTree
    -- childIndices = childrenIndicesByParent LambdaAbstraction vectorTree

    -- -- For each lambda abstraction node, there are 2 child nodes
    -- childGroups :: Acc (Matrix Int)
    -- childGroups = reshape (lift $ Z :. length lambdaNodeIndices :. (2 :: Int)) childIndices

    -- vars = slice childGroups (constant (Z :. All :. (0 :: Int)))
    -- body = slice childGroups (constant (Z :. All :. (1 :: Int)))
    vars = nthChildIndices LambdaAbstraction 0 vectorTree
    body = nthChildIndices LambdaAbstraction 1 vectorTree

    lambdaNodeTypes = map (types !!) lambdaNodeIndices
    varTypes = map (types !!) vars
    bodyTypes = map (types !!) body

    -- Number of function types currently stored
    len = length args

    newArgs = args ++ varTypes
    newRes = res ++ bodyTypes

    newTypeMatrix = T3 types newArgs newRes
    constraints = imap (\(I1 i) v -> lift (v, (functionTag, len + i))) lambdaNodeTypes
    newConstraintMatrix = constraintMatrix ++ constraints


applicationTyping :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
applicationTyping vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, newConstraintMatrix)
  where
    T3 types args res = typeMatrix
    applicationNodeIndices = indicesByNodeType Application vectorTree
    -- childIndices = childrenIndicesByParent Application vectorTree

    -- -- For each function application node, there are 2 child nodes
    -- childGroups :: Acc (Matrix Int)
    -- childGroups = reshape (lift $ Z :. length applicationNodeIndices :. (2 :: Int)) childIndices

    -- funs = slice childGroups (constant (Z :. All :. (0 :: Int)))
    -- funArgs = slice childGroups (constant (Z :. All :. (1 :: Int)))
    funs = nthChildIndices Application 0 vectorTree
    funArgs = nthChildIndices Application 1 vectorTree

    funNodeTypes = map (types !!) funs
    funArgNodeTypes = map (types !!) funArgs
    applicationNodeTypes = map (types !!) applicationNodeIndices

    -- Number of function types currently stored
    len = length args

    -- "@ f x" means that all f's should be functions from x's into @'s
    moreArgs = funArgNodeTypes
    moreRes = applicationNodeTypes

    newArgs = args ++ moreArgs
    newRes = res ++ moreRes
    newTypeMatrix = T3 types newArgs newRes

    constraints = imap (\(I1 i) v -> lift (v, (functionTag, len + i))) funNodeTypes
    newConstraintMatrix = constraintMatrix ++ constraints

-- | When all constraints are collected, resolve them
resolveConstraints :: Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc TypeMatrix, Acc ConstraintMatrix)
resolveConstraints typeMatrix constraintMatrix =
  let
    T3 types args res = typeMatrix

    noRepeats = removeRepeats constraintMatrix
    expanded = expandFunctionEquality noRepeats (zip args res)
    leftified = leftifyTypeVariables expanded
    (indicies, values) = generateTypeMap leftified

    updatedTypeMatrix = updateTypeMatrix typeMatrix indicies values
    T3 newTypes _ _ = updatedTypeMatrix

    substituted = substituteTypeVariables newTypes leftified

    -- TODO: check errors and iterate
  in (updatedTypeMatrix, substituted)

