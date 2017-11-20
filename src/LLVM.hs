{-# LANGUAGE RecordWildCards #-}
module LLVM where

import AST hiding (Type)
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.Module as Module
import qualified LLVM.Internal.Context as Context
import qualified LLVM.AST.Constant as AST hiding (GetElementPtr)
import Codegen
import Data.Maybe
import Data.Word
import Data.String
import Data.Char
import qualified Data.Map as Map

writeModule :: FilePath -> AST.Module -> IO ()
writeModule fp m =
  Context.withContext (\context -> Module.withModuleFromAST context m (\m' -> write context m'))
  where write context m' = Module.writeLLVMAssemblyToFile (Module.File fp) m'

baseMod = runLLVM (emptyModule "WebLang") $ do {
  external (AST.PointerType (AST.IntegerType 32) (AST.AddrSpace 0)) "json" [( AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0), AST.Name (fromString "s"))];
  external (AST.IntegerType 32) "puts" [( AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0)
                                        , AST.Name (fromString "s"))];
  external (AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0)) "jgets" [( AST.PointerType (AST.IntegerType 32) (AST.AddrSpace 0), AST.Name (fromString "s")), (AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0), AST.Name (fromString "s"))];
  external (AST.IntegerType 32) "test" [( AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0), AST.Name (fromString "s"))]
}


buildLLVM :: Program -> AST.Module
buildLLVM p = runLLVM baseMod (simpleLLVM p)

simpleLLVM :: Program -> LLVM ()
simpleLLVM (Program _ _ _ fns) = mapM_ simpleLLVMFunction fns

simpleLLVMFunction :: (FnName, Function) -> LLVM ()
simpleLLVMFunction ("main", (Function {..})) = define llvmRetType "main" [] llvmBody
  where llvmRetType = (AST.IntegerType 32)  
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          maybe (return Nothing) (Just <$>) (simpleLLVMExpressions body) >>= ret
simpleLLVMFunction _ = return ()

concatMaybeCodegen :: ([b] -> b) -> (a -> Maybe (Codegen b)) -> [a] -> Maybe (Codegen b)
concatMaybeCodegen reduce f xs = case catMaybes $ map f xs of
  [] -> Nothing
  res -> Just . (reduce <$>) . sequence $ res

simpleLLVMExpressions :: ExpressionBlock -> Maybe (Codegen AST.Operand)
simpleLLVMExpressions exprs = concatMaybeCodegen last simpleLLVMExpression exprs

simpleLLVMExpression :: (Int, Expression) -> Maybe (Codegen AST.Operand)
simpleLLVMExpression (2, Unassigned term) = simpleLLVMTerm term
simpleLLVMExpression (2, Assignment _ term) = simpleLLVMTerm term
simpleLLVMExpression _ = Nothing

simpleLLVMTerm :: Term -> Maybe (Codegen AST.Operand)
simpleLLVMTerm (FunctionCall fname arg) = simpleLLVMFunctionCall fname arg
simpleLLVMTerm (Literal (ArrVal ts)) = concatMaybeCodegen last simpleLLVMTerm ts
simpleLLVMTerm (Literal (ObjVal ts)) = concatMaybeCodegen last simpleLLVMTerm $ Map.elems ts
simpleLLVMTerm _ = Nothing

simpleLLVMFunctionCall :: String -> Term -> Maybe (Codegen AST.Operand)
simpleLLVMFunctionCall "log" (Literal val) = Just $ llvmLog val
simpleLLVMFunctionCall "jn" (Literal val) = Just $ llvmJson val
simpleLLVMFunctionCall "gets" (Literal val) = Just $ llvmJgets val
simpleLLVMFunctionCall _ _ = Nothing

llvmLog :: PrimValue -> Codegen AST.Operand
llvmLog (StrVal s) = do
  op <- llvmAllocValue (StrVal s)
  call (externf (AST.Name (fromString "puts"))) [op]

llvmLog (ArrVal arr) = if checkNumArgs(2, ArrVal arr) 
  then do
    let codeGens = llvmAllocValues (ArrVal arr)
    ops1 <- codeGens!!0
    ops2 <- codeGens!!1
    call (externf (AST.Name (fromString "puts"))) [ops1]
    call (externf (AST.Name (fromString "puts"))) [ops2]
  else
    error "arrays for log can only have 2 elements"

llvmAllocValue :: PrimValue
               -> Codegen AST.Operand -- pointer to allocated memory
llvmAllocValue (StrVal s) = do
  let ptr = AST.Alloca (llvmCharArrayType (length s + 1)) (Just (cons (AST.Int 32 (fromIntegral 1)))) 0 [] 
  op <- instr $ ptr
  let ref = AST.GetElementPtr True op [cons $ AST.Int 32 0, cons $ AST.Int 32 0] []
  let arrayS = stringToLLVMString s
  _ <- instr $ AST.Store False op (cons arrayS) Nothing 0 []
  op2 <- instr $ ref
  return op2

llvmAllocValue (NumVal num) = undefined--do
   --let ptr = AST.Alloca double Nothing 0 []
   --op <- instr $ ptr
   --let a = 19.2
   --_ <- instr $ AST.Store False op (AST.LocalReference (AST.Name a)) Nothing 0 []
   --return op

llvmAllocValues :: PrimValue -> [Codegen AST.Operand]
llvmAllocValues (ArrVal arr) = do
  let matchHelp x = case x of 
                (Literal x) -> llvmAllocValue x
                _ -> x
  let ptrs = map matchHelp arr
  ptrs
-- etc, for all primative types

--llvmArrayToPointer :: AST.Constant -> AST.Constant
--llvmArrayToPointer arr = AST.GetElementPtr True arr [AST.Int 32 0]

llvmCharArrayType :: Int -> AST.Type 
llvmCharArrayType n = AST.ArrayType (fromIntegral n :: Word64) (AST.IntegerType 8)

stringToLLVMString :: String -> AST.Constant
stringToLLVMString s = AST.Array (AST.IntegerType 8) (map charToLLVMInt s ++ [AST.Int 8 0]) 

charToLLVMInt :: Char -> AST.Constant
charToLLVMInt = AST.Int 8 . fromIntegral . ord

llvmJson :: PrimValue -> Codegen AST.Operand
llvmJson (StrVal s) = do
  op <- llvmAllocValue (StrVal s)
  call (externf (AST.Name (fromString "json"))) [op]

llvmJgets :: PrimValue -> Codegen AST.Operand
llvmJgets (ArrVal arr) = do
  let codeGens = llvmAllocValues (ArrVal arr)
  ops1 <- codeGens!!0
  ops2 <- codeGens!!1
  call (externf (AST.Name (fromString "puts"))) [ops1, ops2]


checkNumArgs :: (Int, PrimValue) -> Bool
checkNumArgs (numOk, ArrVal args) = 
  if numOk == length args
    then True
  else False
