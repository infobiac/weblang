module Lexer.Types where

data LexToken =
    QuoteToken String
  | OperatorToken String
  | EqualsToken
  | ColonToken
  | ArrowToken
  | LeftSquareBracketToken
  | RightSquareBracketToken
  | LeftCurlyBracketToken
  | RightCurlyBracketToken
  | LeftParenToken
  | RightParenToken
  | CommaToken
  | NullToken
  | VarToken String
  | NewlineToken
  | IndentToken Int
  | NumberToken Double
  deriving (Eq,Show)
