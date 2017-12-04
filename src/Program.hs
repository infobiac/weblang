{-# LANGUAGE DeriveGeneric, DeriveAnyClass, FlexibleInstances #-}
module Program ( module X
               , astToProgram
               , Program (..)
               , ExpressionBlock (..)
               , Expression (..)
               , Term (..)
               , Function (..)
               , PrimValue (..)
               ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified AST as AST
import Control.Monad.State
import Control.Monad.Loops
import AST as X
       ( AST
       , ValName (..)
       , FnName (..)
       , TypeName (..)
       , OperatorName (..)
       , Type (..)
       , NewType (..)
       )


-- for pretty printing
import GHC.Generics
import Text.PrettyPrint.GenericPretty

indentIncrement = 2

astToProgram :: AST -> Program
astToProgram ast = Program {
    customTypes = AST.customTypes ast
  , constants = map (\(n, v) -> (n, transSimpleTerm v)) $ AST.constants ast
  , fnDeclarations = map (\(n, f) -> (n, transFunction f)) $ AST.fnDeclarations ast
  }

transFunction :: AST.Function -> Function
transFunction astFunc = Function {
    inputType = AST.inputType astFunc
  , outputType = AST.inputType astFunc
  , arg = AST.arg astFunc
  , body = transExpressions $ AST.body astFunc
  , helper = AST.helper astFunc
  }

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
              if n == n'
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
    Just _ -> return []
  return $ IfThenElse (transSimpleTerm t) thenBlock elseBlock
transTerm (n, AST.ForeachIn v t) = do
  doBlock <- takeIndented (n + indentIncrement)
  case doBlock of
    [] -> error $ "Empty body of a ForeachIn block"
    exprs -> return $ ForeachInDo v (transSimpleTerm t) exprs
transTerm (_, t) = return $ transSimpleTerm t

transSimpleTerm :: AST.Term -> Term
transSimpleTerm (AST.Variable v) = Variable v
transSimpleTerm (AST.FunctionCall n a) = FunctionCall n (transSimpleTerm a)
transSimpleTerm (AST.Operator n a b) = Operator n (transSimpleTerm a) (transSimpleTerm b)
transSimpleTerm (AST.Literal v) = Literal (transPrim v)
transSimpleTerm (AST.IfThenElse p a b) =
  IfThenElse (transSimpleTerm a) [Unassigned $ transSimpleTerm a] [Unassigned $ transSimpleTerm b]
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

data Program = Program {
    customTypes :: [(TypeName, NewType)]
  , constants :: [(ValName, Term)]
  , fnDeclarations :: [(FnName, Function)]
  } deriving (Show, Generic, Out)

data Function = Function {
    inputType :: Type
  , outputType :: Type
  , arg :: ValName
  , body :: ExpressionBlock
  , helper :: Bool
  } deriving (Show, Generic, Out)

type ExpressionBlock = [Expression]

data Expression = Assignment ValName Term
                | Unassigned Term
                deriving (Show, Generic, Out)

data Term = Variable ValName
          | FunctionCall FnName Term
          | Operator OperatorName Term Term
          | Literal PrimValue
          | IfThenElse Term ExpressionBlock ExpressionBlock
          | ForeachInDo ValName Term ExpressionBlock
          deriving (Show, Generic, Out)

data PrimValue = StrVal String
               | NumVal Double
               | ArrVal [Term]
               | ObjVal (Map String Term)
               | NullVal
               deriving (Show, Generic, Out)
