{
module Lexer (
    tokenize
  , LexToken (..)
  ) where

import Lexer.Types
import Lexer.Utils
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n\r\f]
$space = [\ ]
@empty_lines = ($newline ($space* $newline)*)+

tokens :-
  \" ( \n | [^\"\\] | \\. )* \"                                { \s -> QuoteToken (parseQuoted s) }
  "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/"     ;
  ^$space+                                                     { \s -> IndentToken (length s) }
  @empty_lines $space+                                         { \s -> IndentToken (length s - 1) }
  @empty_lines                                                 { \s -> NewlineToken }
  $white+                                                      ;
  "//".*                                                       ;
  \-? $digit+ (\. $digit+)?                                    { \s -> NumberToken (read s) }
  "if"                                                         { \s -> IfToken }
  "then"                                                       { \s -> ThenToken }
  "else"                                                       { \s -> ElseToken }
  "foreach"                                                    { \s -> ForeachToken }
  "type"                                                       { \s -> TypeToken }
  "helper"                                                     { \s -> HelperToken }
  "null"                                                       { \s -> NullToken }
  \[                                                           { \s -> LeftSquareBracketToken }
  \]                                                           { \s -> RightSquareBracketToken }
  \(                                                           { \s -> LeftParenToken }
  \)                                                           { \s -> RightParenToken }
  \{                                                           { \s -> LeftCurlyBracketToken }
  \}                                                           { \s -> RightCurlyBracketToken }
  \,                                                           { \s -> CommaToken }
  \=                                                           { \s -> EqualsToken }
  \:                                                           { \s -> ColonToken }
  "->"                                                         { \s -> ArrowToken }
  [\+\-\*\/]+                                                  { \s -> OperatorToken s }
  $alpha [$alpha $digit \_ \']*                                { \s -> VarToken s }

{
tokenize :: String -> [LexToken]
tokenize = normalizeNewlines . alexScanTokens
-- "/*" ($newline | [^\*] | \* + ($newline | [^/]))* "*/"  ;
}
