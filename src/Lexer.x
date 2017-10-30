{
module Lexer (
    tokenize
  , LexToken (..)
  ) where

import Lexer.Types
import Lexer.Utils
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n\r\f]
$space = [\ ]
@empty_lines = ($newline ($space* $newline)*)+

tokens :-
  \" ( \n | [^\"\\] | \\. )* \"                                { \pos s -> withPos pos $ QuoteToken (parseQuoted s) }
  "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/"     ;
  ^$space+                                                     { \pos s -> withPos pos $ IndentToken (length s) }
  @empty_lines $space+                                         { \pos s -> withPos pos $ IndentToken (length s - 1) }
  @empty_lines                                                 { \pos s -> withPos pos $ NewlineToken }
  $white+                                                      ;
  "//".*                                                       ;
  \-? $digit+ (\. $digit+)?                                    { \pos s -> withPos pos $ NumberToken (read s) }
  "if"                                                         { \pos s -> withPos pos $ IfToken }
  "then"                                                       { \pos s -> withPos pos $ ThenToken }
  "else"                                                       { \pos s -> withPos pos $ ElseToken }
  "foreach"                                                    { \pos s -> withPos pos $ ForeachToken }
  "in"                                                         { \pos s -> withPos pos $ ForeachToken }
  "type"                                                       { \pos s -> withPos pos $ TypeToken }
  "helper"                                                     { \pos s -> withPos pos $ HelperToken }
  "includes"                                                   { \pos s -> withPos pos $ IncludesToken }
  "null"                                                       { \pos s -> withPos pos $ NullToken }
  \[                                                           { \pos s -> withPos pos $ LeftSquareBracketToken }
  \]                                                           { \pos s -> withPos pos $ RightSquareBracketToken }
  \(                                                           { \pos s -> withPos pos $ LeftParenToken }
  \)                                                           { \pos s -> withPos pos $ RightParenToken }
  \{                                                           { \pos s -> withPos pos $ LeftCurlyBracketToken }
  \}                                                           { \pos s -> withPos pos $ RightCurlyBracketToken }
  \,                                                           { \pos s -> withPos pos $ CommaToken }
  \=                                                           { \pos s -> withPos pos $ EqualsToken }
  \:                                                           { \pos s -> withPos pos $ ColonToken }
  "->"                                                         { \pos s -> withPos pos $ ArrowToken }
  $alpha [$alpha $digit \_ \']*                                { \pos s -> withPos pos $ VarToken s }
  [\+\-\*\/\>\<\=\|]+                                          { \pos s -> withPos pos $ OperatorToken s }

{
tokenize :: String -> [Pos LexToken]
tokenize = normalizeNewlines . alexScanTokens

withPos :: AlexPosn -> a -> Pos a
withPos (AlexPn _ line col) a = Pos line col a
}
