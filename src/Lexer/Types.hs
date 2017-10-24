module Lexer.Types where

data LexToken =
  -- basic components
    VarToken String
  | NewlineToken
  | IndentToken Int
  | OperatorToken String

  | IfToken
  | ThenToken
  | ElseToken

  | ForeachToken
  | InToken
  | DoToken

  | TypeToken
  | HelperToken

  -- syntax symbols
  | EqualsToken
  | ColonToken
  | ArrowToken
  | LeftParenToken
  | RightParenToken

  -- primitives
  | NumberToken Double
  | NullToken
  | QuoteToken String
  | CommaToken
  | LeftSquareBracketToken
  | RightSquareBracketToken
  | LeftCurlyBracketToken
  | RightCurlyBracketToken
  deriving (Eq,Show)
