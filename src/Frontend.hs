module Frontend (getTokens, parse, parseTokens) where

import Lang.Abs ( Program )
import Lang.Lex   ( Token )
import Lang.Par   ( pProgram, myLexer )

-- | Run lexer on the given string
getTokens :: String -> [Token]
getTokens = myLexer

parseTokens :: [Token] -> Either String Program
parseTokens = pProgram

parse :: String -> Either String Program
parse = pProgram . myLexer
