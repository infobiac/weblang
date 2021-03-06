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
type OperatorName = String
type ExpressionBlock = [(Int, Expression)]

data AST = AST {
    includes :: [Includes]
  , customTypes :: [(TypeName, NewType)]
  , constants :: [(ValName, Term)]
  , fnDeclarations :: [(FnName, Function)]
  , imports :: [Import]
  } deriving (Show, Generic, Out)

data Import = Import {
    server :: Term
  } deriving (Show, Generic, Out)

data Includes = Includes {
    sourceAddress :: String
  } deriving (Show, Generic, Out)

data Type = Type {
    parentType :: TypeName
  , predicate :: Maybe Term
  } deriving (Show, Generic, Out)

data NewType = NewType {
    shortType :: Type
  , inhabitant :: ValName
  , longPredicate :: ExpressionBlock
  } deriving (Show, Generic, Out)

data Function = Function {
    inputType :: Type
  , outputType :: Type
  , arg :: ValName
  , body :: ExpressionBlock
  , helper :: Bool
  } deriving (Show, Generic, Out)

data Expression = Assignment ValName Term
                | Unassigned Term
                | Assert Term
                deriving (Show, Generic, Out)

data Term = Variable ValName
          | Accessor Term Term
          | FunctionCall FnName Term
          | OperatorTerm Operator Term Term
          | Literal PrimValue
          | If Term
          | Else
          | IfThenElse Term Term Term
          | ForeachInDo ValName Term Term
          | ForeachIn ValName Term
          | Do
          | TypeCheck Term Type
          | TypeAssert Term Type
          deriving (Show, Generic, Out)

data Operator = Plus
              | Minus
              | Multiply
              | Divide
              | Modulus
              | EQ
              | LEQ
              | GEQ
              | GT
              | LT
              | And
              | Or
              deriving (Show, Generic, Out, Eq, Ord)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               | TrueVal
               | FalseVal
               deriving (Show, Generic, Out)

instance Monoid AST where
  mempty = AST [] [] [] [] []
  mappend (AST ais ats acs afs ams) (AST bis bts bcs bfs bms) =
    AST (ais ++ bis) (ats ++ bts) (acs ++ bcs) (afs ++ bfs) (ams ++ bms)

-- for pretty printing maps
instance (Out a, Out b) => Out (Map a b) where
  docPrec i a = docPrec i (Map.toList a)
  doc a = doc (Map.toList a)
  docList as = docList (map Map.toList as)
