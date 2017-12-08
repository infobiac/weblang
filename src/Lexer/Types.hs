module Lexer.Types where

data Pos a = Pos {
    line :: Int
  , col :: Int
  , unPos :: a
  }

instance (Show a) => Show (Pos a) where
  show (Pos line col a) = show line ++ ":" ++ show col ++ " " ++ show a

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

  | IncludesToken

  -- syntax symbols
  | EqualsToken
  | ColonToken
  | ArrowToken
  | LeftParenToken
  | RightParenToken

  -- primitives
  | NumberToken Double
  | NullToken
  | TrueToken
  | FalseToken
  | QuoteToken String
  | CommaToken
  | LeftSquareBracketToken
  | RightSquareBracketToken
  | LeftCurlyBracketToken
  | RightCurlyBracketToken
  deriving (Eq,Ord,Show)
