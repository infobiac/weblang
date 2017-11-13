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

baseMod = runLLVM (emptyModule "WebLang") $ do
  external (AST.IntegerType 32) "puts" [( AST.PointerType (AST.IntegerType 8) (AST.AddrSpace 0)
                                        , AST.Name (fromString "s"))]
  stringConstant "hw" "hello world"

stringConstant :: String -> String -> LLVM ()
stringConstant label s = globalVar (llvmCharArrayType (length s)) label (stringToLLVMString s)

buildLLVM :: Program -> AST.Module
buildLLVM p = runLLVM baseMod (simpleLLVM p)

simpleLLVM :: Program -> LLVM ()
simpleLLVM (Program _ _ _ fns) = mapM_ simpleLLVMFunction fns

simpleLLVMFunction :: (FnName, Function) -> LLVM ()
simpleLLVMFunction ("main", (Function {..})) = define llvmRetType "main" [] llvmBody
  where llvmRetType = AST.IntegerType 32
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
simpleLLVMFunctionCall "log" (Literal (StrVal str)) = Just $ llvmLog str
simpleLLVMFunctionCall _ _ = Nothing

llvmLog :: String -> Codegen AST.Operand
llvmLog s = do
  let hw = cons $ AST.GlobalReference (llvmCharArrayType 11) (AST.Name (fromString "hw"))
  let ref = AST.GetElementPtr True hw [cons $ AST.Int 32 0, cons $ AST.Int 32 0] []
  op <- instr $ ref
  call (externf (AST.Name (fromString "puts"))) [op]

--llvmArrayToPointer :: AST.Constant -> AST.Constant
--llvmArrayToPointer arr = AST.GetElementPtr True arr [AST.Int 32 0]

llvmCharArrayType :: Int -> AST.Type
llvmCharArrayType n = AST.ArrayType (fromIntegral n :: Word64) (AST.IntegerType 8)

stringToLLVMString :: String -> AST.Constant
stringToLLVMString s = AST.Array (AST.IntegerType 8) (map charToLLVMInt s)

charToLLVMInt :: Char -> AST.Constant
charToLLVMInt = AST.Int 8 . fromIntegral . ord
