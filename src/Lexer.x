{
module Lexer (
    alexScanTokens
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

tokens :-
  \" ( \n | [^\"\\] | \\. )* \"                     { \s -> Quote (parseQuoted s) }
  "/*" [\n .]* "*/"                                 ;
  ^$space+                                          { \s -> Indent (length s) }
  $newline $space*                                  { \s -> Indent (length s - 1) }
  $white+                                           ;
  "//".*                                            ;
  \-? $digit+ (\. $digit+)?                         { \s -> Number (read s) }
  \=                                                { \s -> Equals }
  [\+\-\*\/\(\)]+                                   { \s -> Operator s }
  $alpha [$alpha $digit \_ \']*                     { \s -> Var s }
