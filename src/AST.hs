{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
module AST where

import qualified Data.Map as Map
import Data.Map (Map)

-- for pretty printing
import GHC.Generics
import Text.PrettyPrint.GenericPretty

type ValName = String
type FnName = String
type TypeName = String

data Program = Program {
  fnDeclarations :: [(FnName, Function)]
  } deriving (Show, Generic, Out)

data Function = Function {
    inputType :: TypeName
  , outputType :: TypeName
  , arg :: ValName
  , body :: [(Int, Expression)]
  , helper :: Bool
  } deriving (Show, Generic, Out)

data Expression = Assignment ValName Term
                | Unassigned Term
                deriving (Show, Generic, Out)

data Term = Variable ValName
          | FunctionCall FnName Term
          | Literal PrimValue
          deriving (Show, Generic, Out)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               deriving (Show, Generic, Out)

instance Monoid Program where
  mempty = Program []
  mappend (Program afs) (Program bfs) = Program (afs ++ bfs)

-- for pretty printing maps
instance (Out a, Out b) => Out (Map a b) where
  docPrec i a = docPrec i (Map.toList a)
  doc a = doc (Map.toList a)
  docList as = docList (map Map.toList as)
