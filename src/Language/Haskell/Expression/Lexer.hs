module Language.Haskell.Expression.Lexer (
  Token(..)
, tokenize
) where

data Token = Identifier String
  deriving (Eq, Show)

tokenize :: String -> [Token] 
tokenize input = [Identifier input]
