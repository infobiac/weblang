module AST where

import qualified Data.Map as Map
import Data.Map (Map)

type ValName = String
type FnName = String
type TypeName = String

data Program = Program {
  fnDeclarations :: [(FnName, Function)]
  } deriving Show

data Function = Function {
    inputType :: TypeName
  , outputType :: TypeName
  , arg :: ValName
  , body :: [(Int, Expression)]
  , helper :: Bool
  } deriving Show

data Expression = Assignment ValName Term
                | Unassigned Term
                deriving Show

data Term = Variable ValName
          | FunctionCall FnName Term
          | Literal PrimValue
          deriving Show

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               deriving Show

instance Monoid Program where
  mempty = Program []
  mappend (Program afs) (Program bfs) = Program (afs ++ bfs)
