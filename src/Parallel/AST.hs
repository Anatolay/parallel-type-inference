module Parallel.AST where

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
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

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
type TypeIndex = Int
type Node = (Depth, NodeType, String, TypeIndex)

type VarMap = Map.Map String Int

data ConverterState = ConverterState
  { csVarMap :: VarMap
  , csNodeList :: [Node]
  , csDepth :: Depth
  , csIndex :: Int
  }
type Converter a = State ConverterState a

appendNode :: Node -> Converter ()
appendNode node = do
  state <- get
  let nodeList = csNodeList state
  put state { csNodeList = node : nodeList }

incr :: Converter ()
incr = do
  state <- get
  let ind = csIndex state
  put state { csIndex = ind + 1 }

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
      }

convert :: Exp -> Converter ()
convert (Lambda (Ident x) exp) = do
  depth <- getDepth
  ind <- getIndex
  -- variable x is newly introduced, add to the map
  setTypeInd x (ind + 1)

  appendNode (depth, LambdaAbstraction, "lam", ind)
  appendNode (depth + 1, Variable, x, ind + 1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  convert exp

convert (Succ exp) = do
  depth <- getDepth
  ind <- getIndex
  appendNode (depth, SuccOperator, "succ", ind)

  setIndex $ ind + 1
  
  setDepth $ depth + 1
  convert exp

convert (Pred exp) = do
  depth <- getDepth
  ind <- getIndex
  appendNode (depth, PredOperator, "pred", ind)
  
  setIndex $ ind + 1
  
  setDepth $ depth + 1
  convert exp

convert (Cond cond trueExp falseExp) = do
  depth <- getDepth
  ind <- getIndex
  appendNode (depth, Conditional, "if", ind)
  
  setIndex $ ind + 1

  setDepth $ depth + 1
  convert cond

  setDepth $ depth + 1
  convert trueExp
  
  setDepth $ depth + 1
  convert falseExp

convert (Let (Ident x) val body) = do
  depth <- getDepth
  ind <- getIndex
  -- variable x is newly introduced, add to the map
  setTypeInd x (ind + 1)
  appendNode (depth, LetExpr, "let", ind)
  appendNode (depth + 1, Variable, x, ind+1)

  setIndex $ ind + 2

  setDepth $ depth + 1
  convert val

  setDepth $ depth + 1
  convert body

convert (Appl fun exp) = do
  depth <- getDepth
  ind <- getIndex
  appendNode (depth, Application, "@", ind)

  setIndex $ ind + 1

  setDepth $ depth + 1
  convert fun

  setDepth $ depth + 1
  convert exp

convert Zero = do
  depth <- getDepth
  ind <- getIndex
  appendNode (depth, Constant, "0", ind)
  setIndex $ ind + 1

convert (Var (Ident x)) = do
  depth <- getDepth
  ind <- getIndex
  -- variable x is either free or already introduced
  -- If free, getTypeInd return Nothing and we keep ind
  -- Else we use value returned by getTypeInd
  xInd <- fmap (fromMaybe ind) (getTypeInd x)
  appendNode (depth, Variable, x, xInd)
  setIndex $ ind + 1

toNodeList :: Program -> [Node]
toNodeList (Prog stms)
  = concatMap expToNodeList stms

type Index = Int
type NodeTag = Int
type VectorTree = (Vector Index, Vector Depth, Vector NodeTag, Matrix Char, Vector TypeIndex)

astToVectorTree :: Program -> VectorTree
astToVectorTree ast = (indVec, depthVec, nodeTypeVec, valueMatrix, typeIndVec)
  where
    nodeList = toNodeList ast

    depths = fmap (\(d, _, _, _) -> d) nodeList
    nodeTypes  = fmap (fromEnum . (\(_, t, _, _) -> t)) nodeList
    values = padToEqualLength '\0' $ fmap (\(_, _, v, _) -> v) nodeList
    typeIndeces = fmap (\(_, _, _, i) -> i) nodeList

    maxLength arr = maximum (map length arr)

    -- Row indexes, might be substituted by node coordinates
    indVec      = A.fromList (Z :. length values) [0..]

    depthVec    = A.fromList (Z :. length depths) depths
    nodeTypeVec = A.fromList (Z :. length depths) nodeTypes
    valueMatrix = A.fromList (Z :. length values :. maxLength values) (concat values)
    typeIndVec  = A.fromList (Z :. length depths) typeIndeces

type VectorTreeAcc = Acc VectorTree

vectorTreeToAcc :: VectorTree -> Acc VectorTree
vectorTreeToAcc = A.use

padRight :: Int -> a -> [a] -> [a]
padRight n e arr
  | length arr == n = arr
  | otherwise       = arr ++ replicate (n - length arr) e

padToEqualLength :: a -> [[a]] -> [[a]]
padToEqualLength e arr = map (padRight maxLength e) arr
  where
    maxLength = maximum $ map length arr
