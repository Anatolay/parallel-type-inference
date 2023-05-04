{-# LANGUAGE TypeOperators #-}
module Parallel.Inference.Algo where

import Data.Array.Accelerate as A
import Parallel.Inference.Constraints
import Parallel.Inference.TypeMatrix
import Parallel.Inference.Types
import Parallel.AST
import Lang.Abs (Program)

import qualified Prelude as P

(.>) :: (a -> b) -> (b -> c) -> a -> c
(.>) = P.flip (.)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

infer :: Program -> (Acc TypeMatrix, Acc ConstraintMatrix, Acc (Scalar Bool))
infer = P.uncurry performInference . astToVectorTree

performInference :: VectorTree -> Vector TypeIndex -> (Acc TypeMatrix, Acc ConstraintMatrix, Acc (Scalar Bool))
performInference tree indices = res
  where
    gatherConstraints = P.uncurry bindVars
      .> P.uncurry letBindings
      .> uncurry3 inferConstants
      .> uncurry3 succTyping
      .> uncurry3 lambdaTyping
      .> uncurry3 applicationTyping
      .> uncurry3 conditionals

    (_, typeMatrix, constraintMatrix) = gatherConstraints (use tree, use indices)

    res = resolveConstraints typeMatrix constraintMatrix

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
inferConstants vectorTree typeMatrix constraintMatrix = (vectorTree, typeMatrix, newConstraintMatrix)
  where
    natNodes = indicesByNodeType Constant vectorTree
    T3 types _ _ = typeMatrix

    vars = map (types !!) natNodes
    constraints = zip vars $ fill (shape vars) (lift (nat_, 0 :: Int))
    newConstraintMatrix = constraintMatrix ++ constraints


-- | Infer types of succ and pred expressions
succTyping :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
succTyping vectorTree typeMatrix constraintMatrix = (vectorTree, typeMatrix, newConstraintMatrix)
  where
    T3 types _ _ = typeMatrix

    succNodes = indicesByNodeType SuccOperator vectorTree
    predNodes = indicesByNodeType PredOperator vectorTree
    nodes = succNodes ++ predNodes

    succChildren = nthChildIndices SuccOperator 0 vectorTree
    predChildren = nthChildIndices PredOperator 0 vectorTree
    childNodes = succChildren ++ predChildren

    nodeTypes = map (types !!) nodes
    childTypes = map (types !!) childNodes
    allTypes = nodeTypes ++ childTypes

    constraints = zip allTypes $ fill (shape allTypes) (lift (nat_, 0 :: Int))
    newConstraintMatrix = constraintMatrix ++ constraints

-- | Generate constraints for lambda abstractions. Initialize function type matrix.
lambdaTyping :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
lambdaTyping vectorTree typeMatrix constraintMatrix = (vectorTree, newTypeMatrix, newConstraintMatrix)
  where
    T3 types args res = typeMatrix
    lambdaNodeIndices = indicesByNodeType LambdaAbstraction vectorTree

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

conditionals :: Acc VectorTree -> Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc VectorTree, Acc TypeMatrix, Acc ConstraintMatrix)
conditionals vectorTree typeMatrix constraintMatrix = (vectorTree, typeMatrix, newConstraintMatrix)
  where
    T3 types _ _ = typeMatrix
    conditionalNodeIndices = indicesByNodeType Conditional vectorTree

    conds = nthChildIndices Conditional 0 vectorTree
    trues = nthChildIndices Conditional 1 vectorTree
    falses = nthChildIndices Conditional 2 vectorTree
    
    nodeTypes = map (types !!) conditionalNodeIndices
    condTypes = map (types !!) conds
    trueTypes = map (types !!) trues
    falseTypes = map (types !!) falses

    -- Types of true and false branches must be equal.
    -- Type of condition must be Nat.
    -- Type of the whole if node is the same a one of its branches.
    constraints = zip trueTypes falseTypes ++ zip condTypes (fill (shape condTypes) (lift (nat_, 0 :: Int))) ++ zip trueTypes nodeTypes
    newConstraintMatrix = constraintMatrix ++ constraints

-- | When all constraints are collected, resolve them.
-- Returns updated AST and Constraint Matrix, along with a Boolean indicating presence of erros.
resolveConstraints :: Acc TypeMatrix -> Acc ConstraintMatrix -> (Acc TypeMatrix, Acc ConstraintMatrix, Acc (Scalar Bool))
resolveConstraints typeMatrix constraintMatrix = (newTypeMatrix, newConstraintMatrix, hasErrors)
  where
    iterate :: Acc (TypeMatrix, ConstraintMatrix) -> Acc (TypeMatrix, ConstraintMatrix)
    iterate accPair = lift (newTypeMatrix, newConstraintMatrix)
      where
        T2 tm cm = accPair
        T3 types args res = tm

        noRepeats = removeRepeats cm
        expanded = expandFunctionEquality noRepeats (zip args res)
        leftified = leftifyTypeVariables expanded
        (indicies, values) = generateTypeMap leftified

        newTypeMatrix = updateTypeMatrix tm indicies values
        T3 newTypes _ _ = newTypeMatrix

        newConstraintMatrix = substituteTypeVariables newTypes leftified

    predicate :: Acc (TypeMatrix, ConstraintMatrix) -> Acc (Scalar Bool)
    predicate (T2 _ cm) = zipWith (&&) shouldContinue hasNoErrors
      where
        shouldContinue = hasNonTypeVars cm
        hasNoErrors = checkNoErrors cm

    resolved = awhile predicate iterate $ lift (typeMatrix, constraintMatrix)
    T2 newTypeMatrix newConstraintMatrix = resolved
    hasErrors = checkErrors newConstraintMatrix
