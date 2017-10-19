module Lexer.Types where

data LexToken =
    Quote String
  | Operator String
  | Equals
  | Var String
  | Indent Int
  | Number Double
  deriving (Eq,Show)
