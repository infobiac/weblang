{-# LANGUAGE RecordWildCards #-}
module LLVM where

import AST hiding (Type)
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.Module as Module
import qualified LLVM.Internal.Context as Context
import qualified LLVM.AST.Constant as AST hiding (GetElementPtr)
import Codegen
import Control.Monad
import Data.Maybe
import Data.Word
import Data.String
import Data.Char
import qualified Data.Map as Map

writeModule :: FilePath -> AST.Module -> IO ()
writeModule fp m =
  Context.withContext (\context -> Module.withModuleFromAST context m (\m' -> write context m'))
  where write context m' = Module.writeLLVMAssemblyToFile (Module.File fp) m'

buildModule :: Program -> AST.Module
buildModule p = runLLVM moduleHeader (buildLLVM p)

llvmI32Pointer = (AST.PointerType (AST.IntegerType 32) (AST.AddrSpace 0))
llvmStringPointer = (AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0))

moduleHeader = runLLVM (emptyModule "WebLang") $ do {
  external llvmI32Pointer "json" [(llvmStringPointer, AST.Name (fromString "s"))];
  external (AST.IntegerType 32) "puts" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmStringPointer "jgets" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                     , (llvmStringPointer, AST.Name (fromString "s"))];
  external (AST.IntegerType 32) "test" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "post" [(llvmStringPointer, AST.Name (fromString "s"))];
}

buildLLVM :: Program -> LLVM ()
buildLLVM (Program _ _ _ fns) = mapM_ functionLLVM fns

toSig :: String -> [(AST.Type, AST.Name)]
toSig x = [(llvmStringPointer, AST.Name (fromString x))]

functionLLVM :: (FnName, Function) -> LLVM ()
functionLLVM ("main", (Function {..})) = define llvmRetType "main" [] llvmBody
  where llvmRetType = (AST.IntegerType 32)
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          expressionBlockLLVM body >>= ret . Just

--fix return type
functionLLVM (name, (Function {..})) = define llvmRetType name fnargs llvmBody
  where llvmRetType = (AST.IntegerType 32)
        fnargs = toSig arg
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          expressionBlockLLVM body >>= ret . Just

expressionBlockLLVM :: ExpressionBlock -> Codegen AST.Operand
expressionBlockLLVM exprs = last <$> mapM expressionLLVM exprs

expressionLLVM :: (Int, Expression) -> Codegen AST.Operand
expressionLLVM (2, Unassigned term) = termLLVM term
expressionLLVM (2, Assignment _ term) = termLLVM term
expressionLLVM (3, Unassigned term) = termLLVM term
expressionLLVM e = error $ "unimplemented expression " ++ show e

termLLVM :: Term -> Codegen AST.Operand
termLLVM (FunctionCall fname arg) = do
  op <- termLLVM arg
  functionCallLLVM fname op
termLLVM (Literal prim) = primLLVM prim
termLLVM t = error $ "unimplemented term " ++ show t

primLLVM :: PrimValue -> Codegen AST.Operand
primLLVM (ArrVal arr) = do
  elemPtrs <- mapM termLLVM arr
  ptrArray <- buildPtrArray elemPtrs
  llvmCallJsonArr ptrArray (length elemPtrs)
primLLVM (ObjVal obj) = error "unimplemented: object literals"
primLLVM (StrVal s) = do
  let ptr =
        AST.Alloca (llvmCharArrayType (1+length s)) (Just (cons (AST.Int 32 (fromIntegral 1)))) 0 []
  op <- instr $ ptr
  let ref = AST.GetElementPtr True op [cons $ AST.Int 32 0, cons $ AST.Int 32 0] []
  let arrayS = stringToLLVMString s
  _ <- instr $ AST.Store False op (cons arrayS) Nothing 0 []
  op2 <- instr $ ref
  return op2

llvmCallJsonArr :: AST.Operand -> Int -> Codegen AST.Operand
llvmCallJsonArr elemPtrArray n = call (externf (AST.Name (fromString "jsonArr")))
  [elemPtrArray, (cons $ AST.Int 32 (fromIntegral n))]

functionCallLLVM :: String -> AST.Operand -> Codegen AST.Operand
functionCallLLVM "log" arg = llvmCallLog arg
functionCallLLVM "jn" arg = llvmCallJson arg
functionCallLLVM "clientPost" arg = llvmCallPost arg
functionCallLLVM fnName arg = llvmCallFunc fnName arg

llvmCallJson :: AST.Operand -> Codegen AST.Operand
llvmCallJson op = call (externf (AST.Name (fromString "json"))) [op]

llvmCallLog :: AST.Operand -> Codegen AST.Operand
llvmCallLog op = call (externf (AST.Name (fromString "puts"))) [op]

llvmCallPost :: AST.Operand -> Codegen AST.Operand
llvmCallPost op = call (externf (AST.Name (fromString "post"))) [op]

llvmCallFunc :: String -> AST.Operand -> Codegen AST.Operand
llvmCallFunc fnName op = call (externf (AST.Name (fromString fnName))) [op]


llvmCharArrayType :: Int -> AST.Type
llvmCharArrayType n = AST.ArrayType (fromIntegral n :: Word64) (AST.IntegerType 8)

buildPtrArray :: [AST.Operand] -> Codegen AST.Operand
buildPtrArray ptrs = do
  mem <- instr $
    AST.Alloca llvmI32Pointer (Just (cons (AST.Int 32 (fromIntegral (length ptrs))))) 0 []

  forM_ [0..length ptrs] $ \i -> do
    instr $ AST.Store False mem (ptrs!!i) Nothing (fromIntegral i) []
  return mem

stringToLLVMString :: String -> AST.Constant
stringToLLVMString s = AST.Array (AST.IntegerType 8) (map charToLLVMInt s ++ [AST.Int 8 0])

charToLLVMInt :: Char -> AST.Constant
charToLLVMInt = AST.Int 8 . fromIntegral . ord
