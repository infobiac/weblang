{-# LANGUAGE RecordWildCards, StrictData, Strict #-}
module Semantics where

import Prelude hiding (LT, GT, EQ)
import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List
import System.IO.Unsafe
import System.IO
import System.Exit

import Program

data Context = Context {
    signatures :: Map String (Type', Type')
  , typeMap :: Map String Type'
  , opSignatures :: Map Operator (Type', Type', Type')
  }

type Type' = Maybe PrimType

error' s = unsafePerformIO $ do
  hPutStrLn stderr s
  exitFailure

match :: Type' -> Type' -> Bool
match a b = case (==) <$> a <*> b of
  Nothing -> True
  Just True -> True
  Just False -> False

noMatch a b = not $ match a b

lastOr :: a -> [a] -> a
lastOr x xs = if null xs then x else last xs

checkProgram :: Program -> Bool
checkProgram (Program {..}) = and $
                              (map (checkFunction context . snd) fnDeclarations)
                              ++ (map (checkType context . snd) . Map.toList $ types)
  where signatures = Map.fromList $ map (\(fnName, (Function {inputType = inT, outputType = outT})) ->
                                            (fnName, (Just $ baseType inT, Just $ baseType outT)))
                                    fnDeclarations
        context = Context (signatures `Map.union` builtinSignatures) (fmap (Just . baseType) types) operatorSignatures

checkType :: Context -> Type -> Bool
checkType context (Type {..}) = maybe True (\t ->
                                              error' $ "Expect type predicates to be boolean,"
                                                    ++ " but found one with type " ++ show t)
                                . find (noMatch (Just BoolType))
                                . map last
                                . filter (not . null)
                                . map (\(var, block) ->
                                          evalState (evaluate block) (initialTypes var))
                                $ predicates
  where evaluate body = mapM (checkExpression context) body
        initialTypes var = Map.fromList [(var, Just baseType)]

checkFunction :: Context -> Function -> Bool
checkFunction context@(Context {..}) (Function {..}) = Just (baseType outputType) `match` evalState evaluate initialTypes
  where initialTypes = Map.fromList [(arg, Just $ baseType inputType)]
        evaluate = (lastOr Nothing) <$> mapM (checkExpression context) body

checkExpression :: Context -> Expression -> State (Map String Type') Type'
checkExpression context@(Context {..}) (Assignment var term) = do
  res <- checkTerm context term
  modify $ Map.insert var res
  return res
checkExpression context@(Context {..}) (Unassigned term) = checkTerm context term

checkScopedBlock :: Context -> ExpressionBlock -> State (Map String Type') Type'
checkScopedBlock context expressions = do
  namespace <- get
  res <- lastOr Nothing <$> mapM (checkExpression context) expressions
  return res

checkTerm :: Context -> Term -> State (Map String Type') Type'
checkTerm context@(Context {..}) t = do
  m <- get
  case t of
    Variable var ->
      case var `Map.lookup` m of
        Nothing -> error' $ "Called an unassigned variable " ++ var
        Just varType -> return varType
    Accessor val index -> do
      valType <- checkTerm context val
      indexType <- checkTerm context index
      case valType of
        Just ArrType ->
          if noMatch indexType (Just NumType)
          then error' $ "Attempting to index an array with non-Num value: " ++ show index
          else return Nothing
        Just ObjType ->
          if noMatch indexType (Just StrType)
          then error' $ "Attempting to index an object with non-Str value: " ++ show index
          else return Nothing
        Just _ -> error' $ "Attempting to index a non-collection value: " ++ show val
        Nothing -> return Nothing
    FunctionCall fnName arg -> do
      case fnName `Map.lookup` signatures of
        Nothing -> error' $ "Attempting to use undefined function " ++ fnName
        Just (inType, outType) -> do
          argType <- checkTerm context arg
          if argType `noMatch` inType
            then error' $ "Attempting to call function " ++ show fnName ++
                 " with argument of base type " ++ show argType ++
                 ", but function expects a base type " ++ show (inType)
            else return $ outType
    OperatorTerm op t1 t2 -> do
      case op `Map.lookup` opSignatures of
        Nothing -> error' $ "Attempting to use undefined operator " ++ show op
        Just (arg1Type, arg2Type, retType) -> do
          t1Type <- checkTerm context t1
          if arg1Type `noMatch` t1Type
            then error' $ "Left argument to operator " ++ show op ++ " should be of type "
                 ++ show arg1Type ++ ", but it was of type " ++ show t1Type
            else do t2Type <- checkTerm context t2
                    if arg2Type `noMatch` t2Type
                      then error' $ "Right argument to operator " ++ show op ++ " should be of type "
                           ++ show arg2Type ++ ", but it was of type " ++ show t2Type
                      else return retType
    Literal prim -> return $ checkLiteral prim
    IfThenElse pred block1 block2 -> do
      predType <- checkTerm context pred
      if predType `noMatch` Just BoolType && predType `noMatch` Just NumType
        then error' $ "Can't use a non-bool, non-number value as a predicate in an if statement: " ++ show predType
        else do block1Type <- checkScopedBlock context block1
                block2Type <- checkScopedBlock context block2
                if block1Type `noMatch` block2Type
                  then error' $ "The branches of an if statement have two different return types: "
                       ++ show block1Type ++ " vs " ++ show block2Type
                  else return block1Type
    ForeachInDo var arr block -> do
      arrType <- checkTerm context arr
      if arrType `noMatch` Just ArrType
        then error' $ "Can't loop over the non-array value with type " ++ show arrType
        else do withoutVar <- get
                let withVar = Map.insert var Nothing withoutVar
                put withVar
                res <- checkScopedBlock context block
                put withoutVar
                return (Just ArrType)

checkLiteral :: PrimValue -> Type'
checkLiteral (StrVal _) = Just StrType
checkLiteral (NumVal _) = Just NumType
checkLiteral (ArrVal _) = Just ArrType
checkLiteral (ObjVal _) = Just ObjType
checkLiteral NullVal = Just NullType
checkLiteral TrueVal = Just BoolType
checkLiteral FalseVal = Just BoolType

builtinSignatures = Map.fromList [ ("log", (Nothing, Nothing))
                                 , ("isString", (Nothing, Just BoolType))
                                 , ("isArr", (Nothing, Just BoolType))
                                 , ("isNum", (Nothing, Just BoolType))
                                 , ("isObj", (Nothing, Just BoolType))
                                 , ("isBool", (Nothing, Just BoolType))
                                 , ("jn", (Just StrType, Nothing))
                                 , ("addToObj", (Just ArrType, Just ObjType))
                                 , ("push", (Just ArrType, Nothing))
                                 , ("update", (Just ArrType, Just ArrType))
                                 , ("clientPost", (Nothing, Nothing))
                                 , ("clientGet", (Nothing, Nothing))
                                 ]

operatorSignatures = fmap (\(a, b, c) -> (Just a, Just b, Just c)) operatorSignatures'
operatorSignatures' = Map.fromList [ (Plus, (NumType, NumType, NumType))
                                   , (Minus, (NumType, NumType, NumType))
                                   , (Multiply, (NumType, NumType, NumType))
                                   , (Divide, (NumType, NumType, NumType))
                                   , (Modulus, (NumType, NumType, NumType))
                                   , (EQ, (NumType, NumType, BoolType))
                                   , (LEQ, (NumType, NumType, BoolType))
                                   , (GEQ, (NumType, NumType, BoolType))
                                   , (GT, (NumType, NumType, BoolType))
                                   , (LT, (NumType, NumType, BoolType))
                                   , (And, (BoolType, BoolType, BoolType))
                                   , (Or, (BoolType, BoolType, BoolType))
                                   ]
