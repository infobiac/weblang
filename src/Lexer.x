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
  \" ( \n | [^\"\\] | \\. )* \"                               { \pos s -> withPos pos $ QuoteToken (parseQuoted s) }
  "/*" ( $newline | [^\*] | \*+ ($newline | [^\/]) )* "*/"    ;
  ^$space+                                                    { \pos s -> withPos pos $ IndentToken (length s) }
  $space* @empty_lines $space+                                { \pos -> withPos pos . IndentToken . length . takeWhile (== ' ') . reverse }
  @empty_lines                                                { \pos s -> withPos pos $ NewlineToken }
  $white+                                                     ;
  "//".*                                                      ;
  \-? $digit+ (\. $digit+)?                                   { \pos s -> withPos pos $ NumberToken (read s) }
  \.                                                          { \pos s -> withPos pos $ DotToken }
  "if"                                                        { \pos s -> withPos pos $ IfToken }
  "then"                                                      { \pos s -> withPos pos $ ThenToken }
  "else"                                                      { \pos s -> withPos pos $ ElseToken }
  "foreach"                                                   { \pos s -> withPos pos $ ForeachToken }
  "in"                                                        { \pos s -> withPos pos $ InToken }
  "do"							                                          { \pos s -> withPos pos $ DoToken }
  "type"                                                      { \pos s -> withPos pos $ TypeToken }
  "helper"                                                    { \pos s -> withPos pos $ HelperToken }
  "includes"                                                  { \pos s -> withPos pos $ IncludesToken }
  "assert"                                                    { \pos s -> withPos pos $ AssertToken }
  "import"                                                    { \pos s -> withPos pos $ ImportToken }
  "null"                                                      { \pos s -> withPos pos $ NullToken }
  "true"                                                      { \pos s -> withPos pos $ TrueToken }
  "false"                                                     { \pos s -> withPos pos $ FalseToken }
  \[                                                          { \pos s -> withPos pos $ LeftSquareBracketToken }
  \]                                                          { \pos s -> withPos pos $ RightSquareBracketToken }
  \(                                                          { \pos s -> withPos pos $ LeftParenToken }
  \)                                                          { \pos s -> withPos pos $ RightParenToken }
  \{                                                          { \pos s -> withPos pos $ LeftCurlyBracketToken }
  \}                                                          { \pos s -> withPos pos $ RightCurlyBracketToken }
  \,                                                          { \pos s -> withPos pos $ CommaToken }
  \:\?                                                        { \pos s -> withPos pos $ ColonQueToken }
  \:\!                                                        { \pos s -> withPos pos $ ColonExcToken }
  \:                                                          { \pos s -> withPos pos $ ColonToken }
  "->"                                                        { \pos s -> withPos pos $ ArrowToken }
  $alpha [$alpha $digit \_ \']*                               { \pos s -> withPos pos $ VarToken s }
  \+                                                          { \pos s -> withPos pos $ PlusToken }
  \-                                                          { \pos s -> withPos pos $ MinusToken }
  \*                                                          { \pos s -> withPos pos $ MultiplyToken }
  \/                                                          { \pos s -> withPos pos $ DivideToken }
  \%                                                          { \pos s -> withPos pos $ ModToken }
  \=\=                                                        { \pos s -> withPos pos $ EQToken }
  \=                                                          { \pos s -> withPos pos $ EqualsToken }
  \<\=                                                        { \pos s -> withPos pos $ LEQToken }
  \>\=                                                        { \pos s -> withPos pos $ GEQToken }
  \<                                                          { \pos s -> withPos pos $ LTToken }
  \>                                                          { \pos s -> withPos pos $ GTToken }
  \|\|                                                        { \pos s -> withPos pos $ OrToken }
  \&\&                                                        { \pos s -> withPos pos $ AndToken }

{
tokenize :: String -> [Pos LexToken]
tokenize = normalizeNewlines . alexScanTokens

withPos :: AlexPosn -> a -> Pos a
withPos (AlexPn _ line col) a = Pos line col a
}
