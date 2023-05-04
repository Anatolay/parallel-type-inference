module Frontend (getTokens, parse, parseTokens, parseOrEmpty) where

import Lang.Abs ( Program (Prog) )
import Lang.Lex   ( Token )
import Lang.Par   ( pProgram, myLexer )

-- | Run lexer on the given string
getTokens :: String -> [Token]
getTokens = myLexer

parseTokens :: [Token] -> Either String Program
parseTokens = pProgram

parse :: String -> Either String Program
parse = pProgram . myLexer

parseOrEmpty :: String -> Program
parseOrEmpty src = case parse src of
  Left _ -> Prog []
  Right prog -> prog
