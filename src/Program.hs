{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
module Program ( module X
               , astToProgram
               , Program (..)
               , ExpressionBlock (..)
               , Expression (..)
               , Term (..)
               , Function (..)
               , PrimValue (..)
               , Import (..)
               , Type (..)
               , PrimType (..)
               ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified AST as AST
import Control.Monad.State
import Control.Monad.Loops
import Data.Graph
import Data.Maybe
import Data.List
import AST as X
       ( AST
       , Operator (..)
       , ValName (..)
       , FnName (..)
       , TypeName (..)
       , OperatorName (..)
       , NewType (..)
       )
import GHC.Generics
import Text.PrettyPrint.GenericPretty

data Type = Type {
    predicates :: [(ValName, ExpressionBlock)]
  , baseType :: PrimType
  } deriving (Show, Generic, Out)

type TypeMap = Map TypeName Type

data PrimType = StrType
              | NumType
              | ArrType
              | ObjType
              | NullType
              | BoolType
              deriving (Show, Generic, Out, Eq)

defaultInhabitant = "val"

topologicalOrder :: (Show b, Show a, Ord a) => (b -> [a]) -> [(a, b)] -> [(a, b)]
topologicalOrder f = map (\(b, a, _) -> (a, b)) . map unSCC . stronglyConnCompR . map (\(a, b) -> (b, a, f b))
  where unSCC (AcyclicSCC node) = node
        unSCC (CyclicSCC nodes) =
          error $ "There is a cycle in the type definitions for the types: " ++ show nodes

transTypes :: [(TypeName, AST.NewType)] -> TypeMap
transTypes astTypes = foldl' addType initialTypes ordered
  where ordered = topologicalOrder (\t -> [AST.parentType (AST.shortType t)]) astTypes
        initialTypes = Map.fromList [ ("Str", Type [] StrType)
                                    , ("Num", Type [] NumType)
                                    , ("Arr", Type [] ArrType)
                                    , ("Obj", Type [] ObjType)
                                    , ("Null", Type [] NullType)
                                    , ("Bool", Type [] BoolType)
                                    ]
        addType m (name, astType) = Map.insert name (transType m astType) m

transInlineType :: TypeMap -> AST.Type -> Type
transInlineType m (AST.Type parentName shortPred) =
  case parentName `Map.lookup` m of
    Nothing -> error $ "Type " ++ parentName ++ " not found"
    Just (Type parentPreds baseType) ->
      Type {
          baseType = baseType
        , predicates = parentPreds ++
                       maybeToList ((\term -> ( defaultInhabitant
                                              , [Unassigned $ transSimpleTerm term]))
                                     <$> shortPred)
        }

transType :: TypeMap -> AST.NewType -> Type
transType m (AST.NewType (AST.Type parentName shortPred) valName longPred) =
  case parentName `Map.lookup` m of
    Nothing -> error $ "Parent type " ++ parentName ++ " not found"
    Just (Type parentPreds baseType) ->
      Type {
          baseType = baseType
        , predicates = parentPreds ++
                       [(valName, transExpressions longPred)] ++
                       maybeToList ((\term -> ( defaultInhabitant
                                              , [Unassigned $ transSimpleTerm term]))
                                     <$> shortPred)
        }

indentIncrement = 2

astToProgram :: AST -> Program
astToProgram ast = Program {
    types = types
  , constants = map (\(n, v) -> (n, transSimpleTerm v)) $ AST.constants ast
  , fnDeclarations = map (\(n, f) -> (n, transFunction types f)) $ AST.fnDeclarations ast
  , imports = map transImport $ AST.imports ast
  }
  where types = transTypes $ AST.customTypes ast

transFunction :: TypeMap -> AST.Function -> Function
transFunction types astFunc = Function {
    inputType = transInlineType types $ AST.inputType astFunc
  , outputType = transInlineType types $ AST.outputType astFunc
  , arg = AST.arg astFunc
  , body = transExpressions $ AST.body astFunc
  , helper = AST.helper astFunc
  }

transImport :: AST.Import -> Import
transImport (AST.Import t) = Import $ transSimpleTerm t

transExpressions :: AST.ExpressionBlock -> ExpressionBlock
transExpressions = evalState (whileJust transExpression return)

takeNext :: State [a] (Maybe a)
takeNext = do
  ls <- get
  case ls of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

takeIndented :: Int -> State AST.ExpressionBlock ExpressionBlock
takeIndented n = transExpressions <$> takeIndented'
  where takeIndented' = do
          next <- takeNext
          case next of
            Nothing -> return []
            Just expr@(n', _) ->
              if n' >= n
              then do
                rest <- takeIndented'
                return $ expr : rest
              else do
                modify (expr:)
                return $ []

transExpression :: State AST.ExpressionBlock (Maybe Expression)
transExpression = do
  next <- takeNext
  case next of
    Nothing -> return Nothing
    Just (n, AST.Assignment v t) -> (Just . Assignment v) <$> transTerm (n, t)
    Just (n, AST.Unassigned t) -> (Just . Unassigned) <$> transTerm (n, t)

transTerm :: (Int, AST.Term) -> State AST.ExpressionBlock Term
transTerm (n, AST.If t) = do
  thenBlock <- takeIndented (n + indentIncrement)
  next <- takeNext
  elseBlock <- case next of
    Nothing -> return []
    Just (elseInc, AST.Unassigned AST.Else) ->
      if elseInc /= n
      then error $ "Found an else expression with indent " ++ show elseInc ++ ", expected indent " ++ show n
      else takeIndented (n + indentIncrement)
    Just x -> do
      modify (x:)
      return []
  return $ IfThenElse (transSimpleTerm t) thenBlock elseBlock
transTerm (n, AST.ForeachIn v t) = do
  doBlock <- takeIndented (n + indentIncrement)
  case doBlock of
    [] -> error $ "Empty body of a ForeachIn block"
    exprs -> return $ ForeachInDo v (transSimpleTerm t) exprs
transTerm (_, t) = return $ transSimpleTerm t

transSimpleTerm :: AST.Term -> Term
transSimpleTerm (AST.Variable v) = Variable v
transSimpleTerm (AST.Accessor a b) = Accessor (transSimpleTerm a) (transSimpleTerm b)
transSimpleTerm (AST.FunctionCall n a) = FunctionCall n (transSimpleTerm a)
transSimpleTerm (AST.OperatorTerm n a b) = OperatorTerm n (transSimpleTerm a) (transSimpleTerm b)
transSimpleTerm (AST.Literal v) = Literal (transPrim v)
transSimpleTerm (AST.IfThenElse p a b) =
  IfThenElse (transSimpleTerm p) [Unassigned $ transSimpleTerm a] [Unassigned $ transSimpleTerm b]
transSimpleTerm t@(AST.If _) = error $ "unexpected If term: " ++ show t
transSimpleTerm (AST.Else) = error "unexpected Else term"
transSimpleTerm t@(AST.ForeachIn _ _) = error $ "unexpected ForeachIn term: " ++ show t
transSimpleTerm (AST.Do) = error "unexpected Do term"

transPrim :: AST.PrimValue -> PrimValue
transPrim (AST.StrVal s) = (StrVal s)
transPrim (AST.NumVal s) = (NumVal s)
transPrim (AST.ArrVal s) = (ArrVal (map transSimpleTerm s))
transPrim (AST.ObjVal s) = (ObjVal (fmap transSimpleTerm s))
transPrim AST.NullVal = NullVal
transPrim AST.TrueVal = TrueVal
transPrim AST.FalseVal = FalseVal

data Program = Program {
    types :: TypeMap
  , constants :: [(ValName, Term)]
  , fnDeclarations :: [(FnName, Function)]
  , imports :: [Import]
  } deriving (Show, Generic, Out)

data Function = Function {
    inputType :: Type
  , outputType :: Type
  , arg :: ValName
  , body :: ExpressionBlock
  , helper :: Bool
  } deriving (Show, Generic, Out)

data Import = Import Term
            deriving (Show, Generic, Out)

type ExpressionBlock = [Expression]

data Expression = Assignment ValName Term
                | Unassigned Term
                deriving (Show, Generic, Out)

data Term = Variable ValName
          | Accessor Term Term
          | FunctionCall FnName Term
          | OperatorTerm Operator Term Term
          | Literal PrimValue
          | IfThenElse Term ExpressionBlock ExpressionBlock
          | ForeachInDo ValName Term ExpressionBlock
          deriving (Show, Generic, Out)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               | TrueVal
               | FalseVal
               deriving (Show, Generic, Out)
