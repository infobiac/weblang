module AST where

import qualified Data.Map as Map
import Data.Map (Map)

data Program = Program {
  fnDeclarations :: [(Name, Function)]
  } deriving Show

instance Monoid Program where
  mempty = Program []
  mappend (Program afs) (Program bfs) = Program (afs ++ bfs)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               deriving Show

type Name = String
type TypeName = String

data Term = Variable Name
          | FunctionCall Name Term
          | Literal PrimValue
          deriving Show

data Expression = Assignment Name Term
                | Unassigned Term
                deriving Show

data Function = Function {
    inputType :: TypeName
  , outputType :: TypeName
  , arg :: Name
  , body :: [(Int, Expression)]
  } deriving Show
