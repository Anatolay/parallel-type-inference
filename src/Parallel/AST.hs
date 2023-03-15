module Parallel.AST
  ( Depth
  , Parent
  , TypeIndex
  , Index
  , NodeTag
  , Node
  , NodeType(..)
  , VectorTree
  , astToVectorTree
  , astToVectorTree'
  , indicesByNodeType
  , nthChildIndices
  ) where

import Data.Array.Accelerate
  ( Matrix
  , Vector
  , Acc
  , Z(Z)
  , (:.) ((:.))
  )
import qualified Data.Array.Accelerate as A
import Lang.Abs (Exp(..), Ident(Ident), Program(Prog))
import Parallel.Utils ( padToEqualLength )

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (unzip6)

data NodeType
  = Variable
  | LambdaAbstraction
  | Application
  | LetExpr
  | Conditional
  | SuccOperator
  | PredOperator
  | Constant
  deriving (Eq, Enum)

type Depth = Int
type Parent = Int
type ChildIndex = Int
type TypeIndex = Int
type Node = (Depth, Parent, ChildIndex, NodeType, String, TypeIndex)

type VarMap = Map.Map String Int

data ConverterState = ConverterState
  { csVarMap :: VarMap
  , csNodeList :: [Node]
  , csDepth :: Depth
  , csIndex :: Int
  , csParent :: Parent
  , csChildIndex :: ChildIndex
  }
type Converter a = State ConverterState a

appendNode :: Node -> Converter ()
appendNode node = do
  state <- get
  let nodeList = csNodeList state
  put state { csNodeList = node : nodeList }


getTypeInd :: String -> Converter (Maybe TypeIndex)
getTypeInd var = do
  varMap <- gets csVarMap
  return $ Map.lookup var varMap

setTypeInd :: String -> Int -> Converter ()
setTypeInd var ind = do
  state <- get
  let varMap = csVarMap state
  put state { csVarMap = Map.insert var ind varMap }

getVarMap :: Converter VarMap
getVarMap = do
  gets csVarMap

putVarMap :: VarMap -> Converter ()
putVarMap varMap = do
  state <- get
  put state { csVarMap = varMap }

getIndex :: Converter Int
getIndex = gets csIndex

setIndex :: Int -> Converter ()
setIndex ind = do
  state <- get
  put state { csIndex = ind }

getDepth :: Converter Depth
getDepth = gets csDepth

setDepth :: Depth -> Converter ()
setDepth depth = do
  state <- get
  put state { csDepth = depth }

getParent :: Converter Parent
getParent = gets csParent

setParent :: Parent -> Converter ()
setParent par = do
  state <- get
  put state { csParent = par }

getChildIndex :: Converter ChildIndex
getChildIndex = gets csChildIndex

setChildIndex :: ChildIndex -> Converter ()
setChildIndex ci = do
  state <- get
  put state { csChildIndex = ci }

noChild = -1
firstChild = 0
secondChild = 1
thirdChild = 2

expToNodeList :: Exp -> [Node]
expToNodeList exp = nodeList
  where
    nodeList = reverse $ csNodeList finalState
    (_, finalState) = runState (convert exp) initState
    initState = ConverterState
      { csVarMap = Map.empty
      , csNodeList = []
      , csDepth = 0
      , csIndex = 0
      , csParent = -1
      , csChildIndex = -1
      }

convert :: Exp -> Converter ()
convert (Lambda (Ident x) exp) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  -- lambda expresision introduces a new scope.
  -- It is possible that `x` existed in  the previous scope, therefore
  -- we keep the scope and introduce a new one.
  varMap <- getVarMap -- contains old scope
  setTypeInd x (ind + 1) -- updates/creates variable `x`

  appendNode (depth, parent, childIndex, LambdaAbstraction, "lam", ind)
  appendNode (depth + 1, ind, firstChild, Variable, x, ind + 1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  setParent ind
  setChildIndex secondChild
  convert exp

  -- now is the time to restore the old scope.
  putVarMap varMap

convert (Succ exp) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  appendNode (depth, parent, childIndex, SuccOperator, "succ", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  setChildIndex firstChild
  convert exp

convert (Pred exp) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  appendNode (depth, parent, childIndex, PredOperator, "pred", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  setChildIndex firstChild
  convert exp

convert (Cond cond trueExp falseExp) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  appendNode (depth, parent, childIndex, Conditional, "if", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  setChildIndex firstChild
  convert cond

  setDepth $ depth + 1
  setParent ind
  setChildIndex secondChild
  convert trueExp

  setDepth $ depth + 1
  setParent ind
  setChildIndex thirdChild
  convert falseExp

convert (Let (Ident x) val body) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex

  appendNode (depth, parent, childIndex, LetExpr, "let", ind)
  appendNode (depth + 1, ind, firstChild, Variable, x, ind+1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  setParent ind
  setChildIndex secondChild
  convert val

  -- let binding introduces a new scope.
  -- It is possible that `x` existed in  the previous scope, therefore
  -- we keep the scope and introduce a new one.
  --
  -- It is done after converting the value as value can contain references to old `x`
  varMap <- getVarMap -- contains old scope
  setTypeInd x (ind + 1) -- updates/inserts value of `x`

  setDepth $ depth + 1
  setParent ind
  setChildIndex thirdChild
  convert body

  -- now is the time to restore the old scope.
  putVarMap varMap

convert (Appl fun exp) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  appendNode (depth, parent, childIndex, Application, "@", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  setChildIndex firstChild
  convert fun

  setDepth $ depth + 1
  setParent ind
  setChildIndex secondChild
  convert exp

convert Zero = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  appendNode (depth, parent, childIndex, Constant, "0", ind)
  setIndex $ ind + 1

convert (Var (Ident x)) = do
  depth <- getDepth
  parent <- getParent
  childIndex <- getChildIndex
  ind <- getIndex
  -- variable x is either free or already introduced
  -- If free, getTypeInd return Nothing and we keep ind
  -- Else we use value returned by getTypeInd
  xInd <- fmap (fromMaybe ind) (getTypeInd x)
  appendNode (depth, parent, childIndex, Variable, x, xInd)
  setIndex $ ind + 1

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap expToNodeList stms

type Index = Int
type NodeTag = Int
type VectorTree = (Vector Index, Vector Depth, Vector Parent, Vector ChildIndex, Vector NodeTag, Matrix Char)

-- | Given a recursive AST, construct data-parallel AST.
-- Returns constructed AST and initial type variables.
astToVectorTree :: Program -> (VectorTree, Vector TypeIndex)
astToVectorTree ast = ((indVec, depthVec, parentVec, childIndVec, nodeTypeVec, valueMatrix), typeIndVec)
  where
    nodeList = toNodeList ast

    ( depths, parents, childIndices, nodeTypes', values', typeIndeces ) = unzip6 nodeList
    nodeTypes = fmap fromEnum nodeTypes'
    values = padToEqualLength '\0' values'

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indVec      = A.fromList (Z :. length values) [0..]

    depthVec    = A.fromList (Z :. length depths) depths
    parentVec   = A.fromList (Z :. length depths) parents
    childIndVec = A.fromList (Z :. length depths) childIndices
    nodeTypeVec = A.fromList (Z :. length depths) nodeTypes
    valueMatrix = A.fromList (Z :. length values :. maxLength values) (concat values)
    typeIndVec  = A.fromList (Z :. length depths) typeIndeces

-- | Construct data-parallel AST from recursive AST.
astToVectorTree' :: Program -> VectorTree
astToVectorTree' p = let (vectorTree, _) = astToVectorTree p in vectorTree

-- | Given a Node Type and tabular AST, get indeces of rows of this type
-- e.g. select all Succ nodes
indicesByNodeType :: NodeType -> Acc VectorTree -> Acc (Vector Int)
indicesByNodeType nodeType ast = filteredIndices
  where
    tag :: A.Exp NodeTag
    tag = A.lift $ fromEnum nodeType

    hasTag :: A.Exp (Int, NodeTag) -> A.Exp Bool
    hasTag (A.T2 ind tg) = tag A.== tg

    A.T6 indices _ _ _ tags _ = ast
    A.T2 filtered _ = A.filter hasTag $ A.zip indices tags

    (filteredIndices, _) = A.unzip filtered

-- | Given a parent node type and child index, provide indices of all child rows
--
-- e.g. for Cond nodes: condition, true and false branches are the first, second and third children
nthChildIndices :: NodeType -> Int -> Acc VectorTree -> Acc (Vector Int)
nthChildIndices parentNodeType n ast = indices
  where
    A.T6 inds deps pars chInds tags vals = ast

    tag :: A.Exp NodeTag
    tag = A.lift $ fromEnum parentNodeType

    predicate :: A.Exp (Int, Parent, ChildIndex) -> A.Exp Bool
    predicate (A.T3 _ parent childInd) =
      (tags A.!! parent) A.== tag
      A.&& (childInd A.== A.lift n)

    A.T2 childRows _ = A.filter predicate $ A.zip3 inds pars chInds
    (childIndices, parentIndices, _) = A.unzip3 childRows

    -- We have received child indices in the order they appear in the ast.
    -- However, it is required to have them in the order their parents appear.
    -- Scattering these values allows to get them in the correct order.
    scaterred = A.scatter parentIndices (A.fill (A.lift $ Z :. A.length pars) (-1 :: A.Exp Int)) childIndices
    A.T2 indices _ = A.filter (A./= -1) scaterred

    -- TODO: there could be a more efficient approach (probably, needs checking)
    -- If this function accepted a vector of parent indices `parentIndicesVec`:
    -- indices = A.map (scaterred !!) parentIndicesVec
