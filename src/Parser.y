{
module Parser (parse) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import Lexer.Types
import AST
}

%name parse
%tokentype  { LexToken }
%error      { happyError }

%token
  quoted    { QuoteToken $$ }
  oper      { OperatorToken $$ }
  '['		    { LeftSquareBracketToken }
  ']'		    { RightSquareBracketToken }
  '{'		    { LeftCurlyBracketToken }
  '}'		    { RightCurlyBracketToken }
  '('		    { LeftParenToken }
  ')'		    { RightParenToken }
  ','		    { CommaToken }
  '='		    { EqualsToken }
  ':'		    { ColonToken }
  arrow	    { ArrowToken }
  var		    { VarToken $$ }
  line		  { NewlineToken }
  indent	  { IndentToken $$ }
  num		    { NumberToken $$ }
  helper    { HelperToken }
  null		  { NullToken }
  if        { IfToken }
  then      { ThenToken }
  else      { ElseToken }
  foreach   { ForeachToken }
  in        { InToken }
  do        { DoToken }
  type      { TypeToken }

%%

Program
  : line TopLevel Program       { $2 <> $3 }
  | line TopLevel               { $2 }

TopLevel
  : FunctionDeclaration        { Program [] [] [$1] }
  | Constant                   { Program [] [$1] [] }
  | CustomType                 { Program [$1] [] [] }

Constant
  : var '=' Term               { ($1, $3) }

CustomType
  : type var var ':' Type              { ($2, NewType $5 $3 []) }
  | type var var ':' Type Expressions  { ($2, NewType $5 $3 $6) }

FunctionDeclaration
  : var var ':' Type arrow Type Expressions           { ($1, Function $4 $6 $2 $7 False) }
  | helper var var ':' Type arrow Type Expressions    { ($2, Function $5 $7 $3 $8 True) }

Type
  : var '[' Term ']'  { Type $1 (Just $3) }
  | var               { Type $1 Nothing }

Expressions
  : indent Expression Expressions { ($1, $2) : $3 }
  | indent Expression             { [($1, $2)] }

Expression
  : var '=' Term  { Assignment $1 $3 }
  | Term          { Unassigned $1 }

Term
  : Term1                 { $1 }
  | foreach var in Term1  { ForeachIn $2 $4 }
  | if Term1              { If $2 }
  | IfThenElse            { $1 }
  | ForeachInDo           { $1 }

Term1
  : var Term0             { FunctionCall $1 $2 }
  | Term1 oper Term0      { Operator $2 $1 $3  }
  | else                  { Else }
  | do                    { Do }
  | Term0                 { $1 }

Term0
  : '(' Term ')'          { $2 }
  | var                   { Variable $1 }
  | Literal               { Literal $1 }

IfThenElse
  : if Term0 then Term else Term1 { IfThenElse $2 $4 $6 }

ForeachInDo
  : foreach var in Term0 do Term1  { ForeachInDo $2 $4 $6 }

Literal
  : quoted                    { (StrVal $1) }
  | num                       { (NumVal $1) }
  | '[' ']'                   { ArrVal [] }
  | '[' indent ']'            { ArrVal [] }
  | '[' ArrayTerms indent ']' { ArrVal $2 }
  | '[' ArrayTerms ']'        { ArrVal $2 }
  | '{' '}'                   { ObjVal Map.empty }
  | '{' ObjectTerms indent '}' { (ObjVal $2) }
  | '{' ObjectTerms '}'       { (ObjVal $2) }
  | null                      { NullVal }

ArrayTerms
  : Term ',' ArrayTerms           { $1 : $3 }
  | indent Term ',' ArrayTerms    { $2 : $4 }
  | indent Term                   { [ $2 ] }
  | Term                          { [ $1 ] }

ObjectTerms
  : var ':' Term ',' ObjectTerms  { Map.insert $1 $3 $5 }
  | indent var ':' Term ',' ObjectTerms  { Map.insert $2 $4 $6 }
  | indent var ':' Term                  { Map.singleton $2 $4 }
  | var ':' Term                  { Map.singleton $1 $3 }

{
happyError :: [LexToken] -> a
happyError ts = error $ "Parse error. Remaining tokens: " ++ show ts ++ "\n"
}
