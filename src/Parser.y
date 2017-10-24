{
module Parser (parse) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import Lexer.Types
import AST
}

%name parse
%tokentype  { Pos LexToken }
%error      { happyError }

%token
  quoted    { Pos _ _ (QuoteToken $$) }
  oper      { Pos _ _ (OperatorToken $$) }
  '['		    { Pos _ _ (LeftSquareBracketToken) }
  ']'		    { Pos _ _ (RightSquareBracketToken) }
  '{'		    { Pos _ _ (LeftCurlyBracketToken) }
  '}'		    { Pos _ _ (RightCurlyBracketToken) }
  '('		    { Pos _ _ (LeftParenToken) }
  ')'		    { Pos _ _ (RightParenToken) }
  ','		    { Pos _ _ (CommaToken) }
  '='		    { Pos _ _ (EqualsToken) }
  ':'		    { Pos _ _ (ColonToken) }
  arrow	    { Pos _ _ (ArrowToken) }
  var		    { Pos _ _ (VarToken $$) }
  line		  { Pos _ _ (NewlineToken) }
  indent	  { Pos _ _ (IndentToken $$) }
  num		    { Pos _ _ (NumberToken $$) }
  helper    { Pos _ _ (HelperToken) }
  null		  { Pos _ _ (NullToken) }
  if        { Pos _ _ (IfToken) }
  then      { Pos _ _ (ThenToken) }
  else      { Pos _ _ (ElseToken) }
  foreach   { Pos _ _ (ForeachToken) }
  in        { Pos _ _ (InToken) }
  do        { Pos _ _ (DoToken) }
  type      { Pos _ _ (TypeToken) }

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
  : quoted                            { (StrVal $1) }
  | num                               { (NumVal $1) }
  | '[' ']'                           { ArrVal [] }
  | '[' indent ']'                    { ArrVal [] }
  | '[' ArrayTerms indent ']'         { ArrVal $2 }
  | '[' indent ArrayTerms ']'         { ArrVal $3 }
  | '[' indent ArrayTerms indent ']'  { ArrVal $3 }
  | '[' ArrayTerms ']'                { ArrVal $2 }
  | '{' '}'                           { ObjVal Map.empty }
  | '{' indent '}'                    { ObjVal Map.empty }
  | '{' ObjectTerms indent '}'        { (ObjVal $2) }
  | '{' indent ObjectTerms '}'        { (ObjVal $3) }
  | '{' indent ObjectTerms indent '}' { (ObjVal $3) }
  | '{' ObjectTerms '}'               { (ObjVal $2) }
  | null                              { NullVal }

ArrayTerms
  : Term ',' ArrayTerms           { $1 : $3 }
  | Term                          { [ $1 ] }

ObjectTerms
  : var ':' Term ',' ObjectTerms  { Map.insert $1 $3 $5 }
  | var ':' Term                  { Map.singleton $1 $3 }

{
happyError :: [Pos LexToken] -> a
happyError (Pos line col t:ts) = error $ "Parse error on token at line " ++ show line ++ " col " ++ show col ++ ". Token:\n    " ++ show t ++ "\n"
}
