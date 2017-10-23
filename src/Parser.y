{
module Parser (parse) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Monoid

import Lexer.Types
import AST
}

%name parse
%tokentype { LexToken }
%error { happyError }

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

%%

Program : line FunctionDeclaration Program     { Program [$2] <> $3 }
        | line FunctionDeclaration             { Program [$2] }

FunctionDeclaration : var var ':' var arrow var Expressions    { ($1, Function $4 $6 $2 $7 False) }
                    | helper var var ':' var arrow var Expressions    { ($2, Function $5 $7 $3 $8 True) }

Expressions : indent Expression Expressions { ($1, $2) : $3 }
            | indent Expression             { [($1, $2)] }

Expression : var '=' Term  { Assignment $1 $3 }
           | Term          { Unassigned $1 }

Term : var NestableTerm    { FunctionCall $1 $2 }
     | NestableTerm        { $1 }

NestableTerm : '(' Term ')'        { $2 }
             | var                 { Variable $1 }
             | Literal             { Literal $1 }

Literal : quoted                { (StrVal $1) }
        | num                   { (NumVal $1) }
        | '[' ']'               { ArrVal [] }
        | '[' ArrayTerms ']'    { ArrVal $2 }
        | '{' '}'               { ObjVal Map.empty }
        | '{' ObjectTerms '}'   { (ObjVal $2) }
        | null                  { NullVal }

ArrayTerms : Term ',' ArrayTerms    { $1 : $3 }
           | Term                   { [ $1 ] }

ObjectTerms : var ':' Term ',' ObjectTerms  { Map.insert $1 $3 $5 }
            | var ':' Term                  { Map.singleton $1 $3 }

{
happyError :: [LexToken] -> a
happyError _ = error ("Parse error\n")
}
