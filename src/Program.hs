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
               , Endpoint (..)
               , Method (..)
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
                                              , [Unassigned $ transSimpleTerm m term]))
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
                       [(valName, transExpressions m longPred)] ++
                       maybeToList ((\term -> ( defaultInhabitant
                                              , [Unassigned $ transSimpleTerm m term]))
                                     <$> shortPred)
        }

indentIncrement = 2

astToProgram :: AST -> Program
astToProgram ast = Program {
    types = types
  , constants = map (\(n, v) -> (n, transSimpleTerm types v)) $ AST.constants ast
  , fnDeclarations = map (\(n, f) -> (n, transFunction types f)) $ AST.fnDeclarations ast
  , imports = map (transImport types) $ AST.imports ast
  }
  where types = transTypes $ AST.customTypes ast

transFunction :: TypeMap -> AST.Function -> Function
transFunction types astFunc = Function {
    inputType = transInlineType types $ AST.inputType astFunc
  , outputType = transInlineType types $ AST.outputType astFunc
  , arg = AST.arg astFunc
  , body = transExpressions types $ AST.body astFunc
  , helper = AST.helper astFunc
  }

transImport :: TypeMap -> AST.Import -> Import
transImport types (AST.Import t) = parseImportArg $ transSimpleTerm types t

transExpressions :: TypeMap -> AST.ExpressionBlock -> ExpressionBlock
transExpressions types = evalState (whileJust (transExpression types) return)

takeNext :: State [a] (Maybe a)
takeNext = do
  ls <- get
  case ls of
    [] -> return Nothing
    (x:xs) -> do
      put xs
      return (Just x)

takeIndented :: TypeMap -> Int -> State AST.ExpressionBlock ExpressionBlock
takeIndented types n = transExpressions types <$> takeIndented'
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

transExpression :: TypeMap -> State AST.ExpressionBlock (Maybe Expression)
transExpression types = do
  next <- takeNext
  case next of
    Nothing -> return Nothing
    Just (n, AST.Assignment v t) -> (Just . Assignment v) <$> transTerm types (n, t)
    Just (n, AST.Unassigned t) -> (Just . Unassigned) <$> transTerm types (n, t)
    Just (n, AST.Assert t) -> (Just . Assert) <$> transTerm types (n, t)

transTerm :: TypeMap -> (Int, AST.Term) -> State AST.ExpressionBlock Term
transTerm types (n, AST.If t) = do
  thenBlock <- takeIndented types (n + indentIncrement)
  next <- takeNext
  elseBlock <- case next of
    Nothing -> return []
    Just (elseInc, AST.Unassigned AST.Else) ->
      if elseInc /= n
      then error $ "Found an else expression with indent " ++ show elseInc ++ ", expected indent " ++ show n
      else takeIndented types (n + indentIncrement)
    Just x -> do
      modify (x:)
      return []
  return $ IfThenElse (transSimpleTerm types t) thenBlock elseBlock
transTerm types (n, AST.ForeachIn v t) = do
  doBlock <- takeIndented types (n + indentIncrement)
  case doBlock of
    [] -> error $ "Empty body of a ForeachIn block"
    exprs -> return $ ForeachInDo v (transSimpleTerm types t) exprs
transTerm types (_, t) = return $ transSimpleTerm types t

transSimpleTerm :: TypeMap -> AST.Term -> Term
transSimpleTerm _ (AST.Variable v) = Variable v
transSimpleTerm types (AST.Accessor a b) = Accessor (transSimpleTerm types a) (transSimpleTerm types b)
transSimpleTerm types (AST.FunctionCall n a) = FunctionCall n (transSimpleTerm types a)
transSimpleTerm types (AST.OperatorTerm n a b) = OperatorTerm
  n (transSimpleTerm types a) (transSimpleTerm types b)
transSimpleTerm types (AST.Literal v) = Literal (transPrim types v)
transSimpleTerm types (AST.TypeCheck v t) = TypeCheck (transSimpleTerm types v) (transInlineType types t)
transSimpleTerm types (AST.TypeAssert v t) = TypeAssert (transSimpleTerm types v) (transInlineType types t)
transSimpleTerm types (AST.IfThenElse p a b) = IfThenElse
  (transSimpleTerm types p) [Unassigned $ transSimpleTerm types a] [Unassigned $ transSimpleTerm types b]
transSimpleTerm types t@(AST.If _) = error $ "unexpected If term: " ++ show t
transSimpleTerm types (AST.Else) = error "unexpected Else term"
transSimpleTerm types t@(AST.ForeachIn _ _) = error $ "unexpected ForeachIn term: " ++ show t
transSimpleTerm types (AST.Do) = error "unexpected Do term"

transPrim :: TypeMap -> AST.PrimValue -> PrimValue
transPrim _ (AST.StrVal s) = (StrVal s)
transPrim _ (AST.NumVal s) = (NumVal s)
transPrim types (AST.ArrVal s) = (ArrVal (map (transSimpleTerm types) s))
transPrim types (AST.ObjVal s) = (ObjVal (fmap (transSimpleTerm types) s))
transPrim _ AST.NullVal = NullVal
transPrim _ AST.TrueVal = TrueVal
transPrim _ AST.FalseVal = FalseVal

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

data Import = Import URL Key Secret [Endpoint]
            deriving (Show, Generic, Out)

type ExpressionBlock = [Expression]

data Expression = Assignment ValName Term
                | Unassigned Term
                | Assert Term
                deriving (Show, Generic, Out)

data Term = Variable ValName
          | Accessor Term Term
          | FunctionCall FnName Term
          | OperatorTerm Operator Term Term
          | Literal PrimValue
          | IfThenElse Term ExpressionBlock ExpressionBlock
          | ForeachInDo ValName Term ExpressionBlock
          | TypeCheck Term Type
          | TypeAssert Term Type
          deriving (Show, Generic, Out)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               | TrueVal
               | FalseVal
               deriving (Show, Generic, Out)


data Method = Post | Get
            deriving (Eq, Show, Generic, Out)
type EndpointFnName = String
type EndpointEndpoint = String
type URL = String
type Key = String
type Secret = String
data Endpoint = Endpoint EndpointFnName EndpointEndpoint Method
              deriving (Show, Generic, Out)

parseImportArg :: Term -> Import
parseImportArg (Literal (ObjVal obj)) = Import url key secret endpoints
  where getVal objName obj key = fromMaybe (error $ key ++ " missing from " ++ objName) (Map.lookup key obj)
        getImpVal = getVal "import statement" obj
        url = case getImpVal "url" of
          (Literal (StrVal url)) -> url
          _ -> error "url key in import statement should be a string value"
        key = case getImpVal "key" of
          (Literal (StrVal key)) -> key
          _ -> error "auth key in import statement missing. If no key is required, use emtpy string"
        secret = case getImpVal "secret" of
          (Literal (StrVal secret)) -> secret
          _ -> error "auth secret in import statement missing. If no secret is required, use emtpy string"
        endpoints = case getImpVal "endpoints" of
          (Literal (ArrVal endpointTerms)) -> flip map endpointTerms $ \t -> case t of
            (Literal (ObjVal endpointObj)) ->
              let getEndpVal = getVal "endpoint statement" endpointObj
                  name = case getEndpVal "fnName" of
                    (Literal (StrVal name)) -> name
                    _ -> error "endpoint's fnName should be a string"
                  endpoint = case getEndpVal "endpoint" of
                    (Literal (StrVal endpoint)) -> endpoint
                    _ -> error "endpoint should be a string"
                  method = case getEndpVal "is_post" of
                    (Literal TrueVal) -> Post
                    (Literal FalseVal) -> Get
                    _ -> error "endpoint is_post should be true/false"
              in Endpoint name endpoint method

            _ -> error "endpoint values in import statement should be object literals"
          _ -> error "endpoint key in import statement should be an array value"
parseImportArg _ = error "Import called with non-primitive object argument"
