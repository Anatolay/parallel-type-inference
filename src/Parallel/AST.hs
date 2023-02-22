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
  , childrenIndicesByParent
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

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (unzip5)

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
type TypeIndex = Int
type Node = (Depth, Parent, NodeType, String, TypeIndex)

type VarMap = Map.Map String Int

data ConverterState = ConverterState
  { csVarMap :: VarMap
  , csNodeList :: [Node]
  , csDepth :: Depth
  , csIndex :: Int
  , csParent :: Int
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
      }

convert :: Exp -> Converter ()
convert (Lambda (Ident x) exp) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  -- variable x is newly introduced, add to the map
  setTypeInd x (ind + 1)

  appendNode (depth, parent, LambdaAbstraction, "lam", ind)
  appendNode (depth + 1, ind, Variable, x, ind + 1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  setParent ind
  convert exp

convert (Succ exp) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  appendNode (depth, parent, SuccOperator, "succ", ind)

  setIndex $ ind + 1
  
  setDepth $ depth + 1
  setParent ind
  convert exp

convert (Pred exp) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  appendNode (depth, parent, PredOperator, "pred", ind)
  
  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  convert exp

convert (Cond cond trueExp falseExp) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  appendNode (depth, parent, Conditional, "if", ind)
  
  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  convert cond

  setDepth $ depth + 1
  setParent ind
  convert trueExp
  
  setDepth $ depth + 1
  setParent ind
  convert falseExp

convert (Let (Ident x) val body) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  -- variable x is newly introduced, add to the map
  setTypeInd x (ind + 1)
  appendNode (depth, parent, LetExpr, "let", ind)
  appendNode (depth + 1, ind, Variable, x, ind+1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  setParent ind
  convert val

  setDepth $ depth + 1
  setParent ind
  convert body

convert (Appl fun exp) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  appendNode (depth, parent, Application, "@", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  setParent ind
  convert fun

  setDepth $ depth + 1
  setParent ind
  convert exp

convert Zero = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  appendNode (depth, parent, Constant, "0", ind)
  setIndex $ ind + 1

convert (Var (Ident x)) = do
  depth <- getDepth
  parent <- getParent
  ind <- getIndex
  -- variable x is either free or already introduced
  -- If free, getTypeInd return Nothing and we keep ind
  -- Else we use value returned by getTypeInd
  xInd <- fmap (fromMaybe ind) (getTypeInd x)
  appendNode (depth, parent, Variable, x, xInd)
  setIndex $ ind + 1

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap expToNodeList stms

type Index = Int
type NodeTag = Int
type VectorTree = (Vector Index, Vector Depth, Vector Parent, Vector NodeTag, Matrix Char)

-- | Given a recursive AST, construct data-parallel AST.
-- Returns constructed AST and initial type variables.
astToVectorTree :: Program -> (VectorTree, Vector TypeIndex)
astToVectorTree ast = ((indVec, depthVec, parentVec, nodeTypeVec, valueMatrix), typeIndVec)
  where
    nodeList = toNodeList ast

    ( depths, parents, nodeTypes', values', typeIndeces ) = unzip5 nodeList
    nodeTypes = fmap fromEnum nodeTypes'
    values = padToEqualLength '\0' values'

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indVec      = A.fromList (Z :. length values) [0..]

    depthVec    = A.fromList (Z :. length depths) depths
    parentVec   = A.fromList (Z :. length depths) parents
    nodeTypeVec = A.fromList (Z :. length depths) nodeTypes
    valueMatrix = A.fromList (Z :. length values :. maxLength values) (concat values)
    typeIndVec  = A.fromList (Z :. length depths) typeIndeces

-- | Construct data-parallel AST from recursive AST.
astToVectorTree' :: Program -> VectorTree
astToVectorTree' p = let (vectorTree, _) = astToVectorTree p in vectorTree

-- UTILS --
padRight :: Int -> a -> [a] -> [a]
padRight n e arr
  | length arr == n = arr
  | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
  where
    maxLength = maximum $ map length arr
-- --

-- | Given a Node Type and tabular AST, get indeces of rows of this type
-- e.g. select all Succ nodes
indicesByNodeType :: NodeType -> Acc VectorTree -> Acc (Vector Int)
indicesByNodeType nodeType ast = filteredIndices
  where
    tag :: A.Exp NodeTag
    tag = A.lift $ fromEnum nodeType

    hasTag :: A.Exp (Int, NodeTag) -> A.Exp Bool
    hasTag (A.T2 ind tg) = tag A.== tg

    A.T5 indices _ _ tags _ = ast
    A.T2 filtered _ = A.filter hasTag $ A.zip indices tags

    (filteredIndices, _) = A.unzip filtered

-- | For each parent index in the input vector, provide indices of all child rows
-- childrenIndicesByParent :: Acc (Vector Int) -> Acc VectorTree -> Acc (Vector (Vector Int))
-- childrenIndicesByParent parents ast = undefined
--   where
--     A.T5 inds deps pars tags vals = ast

--     getChildren :: A.Exp Int -> A.Exp (Vector Int)
--     getChildren ind = fst . A.unzip . A.afst $ A.filter isChild $ A.zip inds pars
--       where
--         isChild :: A.Exp (Int, Parent) -> A.Exp Bool
--         isChild (A.T2 _ p) = p A.== ind

--     a = A.map getChildren parents

-- | Given parent node type, provide indices of all child rows
--
-- Note that we will use the fact that the number of children is always constant
-- for a given parent node, e.g. 3 children for Let node.
childrenIndicesByParent :: NodeType -> Acc VectorTree -> Acc (Vector Int)
childrenIndicesByParent parentNodeType ast = indices
  where
    A.T5 inds deps pars tags vals = ast

    tag :: A.Exp NodeTag
    tag = A.lift $ fromEnum parentNodeType

    predicate :: A.Exp (Int, Parent) -> A.Exp Bool
    predicate (A.T2 _ parent) = (tags A.!! parent) A.== tag

    A.T2 rows _ = A.filter predicate $ A.zip inds pars
    (indices, _) = A.unzip rows

