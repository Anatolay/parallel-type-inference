{-# LANGUAGE TypeOperators #-}

module Print where

import Data.Array.Accelerate (toList, (:.)((:.)), DIM1, arrayShape)
import Frontend (getTokens, parseTokens)
import qualified Lang.Print (Print, printTree)
import Parallel.AST ( VectorTree, astToVectorTree' )
import System.Exit (exitFailure, exitSuccess)

-- | Same as `run`, but accepts a program's source file path instead of a String
runFile :: FilePath -> IO ()
runFile f = putStrLn f >> readFile f >>= run

-- | Attempt to parse an input string and print resulting AST
run :: String -> IO ()
run s = case parseTokens tokens of
  Left error -> do
    putStrLn "\nParse              Failed...\n"
    putStrLn "Tokens:"
    print tokens
    putStrLn error
    exitFailure
  Right tree -> do
    putStrLn "\nParse Successful!"
    printTree tree
    exitSuccess
  where
  tokens = getTokens s

-- | Turn AST into (somewhat) prettified String
showTree :: (Lang.Print.Print a) => a -> String
showTree = Lang.Print.printTree

-- | Same as `showTree`, but outputs to stdout
printTree :: (Show a, Lang.Print.Print a) => a -> IO ()
printTree tree
 = do
      putStrLn $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrLn $ "\n[Linearized tree]\n\n" ++ showTree tree

showVectorTree :: VectorTree -> String
showVectorTree (i, d, p, ci, nt, v) =
  let
    charMatrixToLines cm = go (toList cm) [] 0
      where
        _ :. length = arrayShape cm :: (DIM1 :. Int)

        -- get rid of paddings
        go [] lst 0 = []
        go [] lst _ = [reverse lst]
        go ('\0' : rest) lst 0 = go rest [] 0
        go ('\0' : rest) lst _ = reverse lst : go rest [] 0
        go (c : rest) lst len
          | len == length = reverse lst : go rest [c] 1
          | otherwise = go rest (c : lst) (len + 1)

    combineLines :: [[String]] -> [String]
    combineLines = foldl1 (zipWith (\x y -> x ++ ('\t' : y)))

    i' = show <$> toList i
    d' = show <$> toList d
    p' = show <$> toList p
    ci' = show <$> toList ci
    nt' = show <$> toList nt
    v' = charMatrixToLines v

    matrixRows = combineLines [i', d', p', ci', nt', v']
  in unlines ("Vectorized AST:" : matrixRows)

printVectorTree :: VectorTree -> IO ()
printVectorTree = putStrLn . showVectorTree

runVectorTree :: String -> IO ()
runVectorTree s = case parseTokens tokens of
  Left error -> do
    putStrLn "\nParse              Failed...\n"
    putStrLn "Tokens:"
    print tokens
    putStrLn error
    exitFailure
  Right ast -> do
    putStrLn "\nParse Successful!"
    printTree ast
    putStrLn ""
    printVectorTree $ astToVectorTree' ast
    exitSuccess
  where
  tokens = getTokens s
