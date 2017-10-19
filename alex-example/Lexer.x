{
module Lexer (
    alexScanTokens
  ) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$newline = [\n\r\f]
$space = [\ ]

tokens :-
  \" ( \n | [^\"\\] | \\. )* \"                     { \s -> Quote (tail . init $ s) }
  "/*" [\n .]* "*/"                                 ;
  ^$space+                                          { \s -> Indent (length s) }
  $newline $space*                                  { \s -> Indent (length s - 1) }
  $white+                                           ;
  "//".*                                            ;
  \-? ( $digit+ \.? $digit* | $digit* \.? $digit+ ) { \s -> Number s }
  \=                                                { \s -> Equals }
  [\+\-\*\/\(\)]+                                   { \s -> Operator s }
  $alpha [$alpha $digit \_ \']*                     { \s -> Var s }
{

data Token =
        Quote String    |
        Operator String |
        Equals          |
        Var String      |
        Indent Int      |
        Number String
        deriving (Eq,Show)

}
