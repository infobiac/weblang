{-# LANGUAGE RecordWildCards #-}
module LLVM where

import Prelude hiding (EQ, LEQ, GEQ, GT, LT)
import Program hiding (Type)
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.Module as Module
import qualified LLVM.Internal.Context as Context
import qualified LLVM.AST.Constant as AST hiding (GetElementPtr, FCmp, ICmp, PtrToInt, FPToUI)
import qualified LLVM.AST.FloatingPointPredicate as Floatypoo
import qualified LLVM.AST.IntegerPredicate as Intypoo
import qualified LLVM.AST.Float as Fl
import Codegen
import Control.Monad
import Control.Monad.State
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

llvmI8 = AST.IntegerType 8
llvmI32 = AST.IntegerType 32
llvmI32Pointer = (AST.PointerType llvmI32 (AST.AddrSpace 0))
llvmI32PointerPointer = (AST.PointerType llvmI32Pointer (AST.AddrSpace 0))
llvmStringPointer = (AST.PointerType llvmI8 (AST.AddrSpace 0))
llvmPointerStringfPointer = (AST.PointerType llvmStringPointer (AST.AddrSpace 0))
llvmDouble = AST.FloatingPointType AST.DoubleFP

moduleHeader = runLLVM (emptyModule "WebLang") $ do
  external llvmI32Pointer "json" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32 "puts" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32 "floor" [(llvmDouble, AST.Name (fromString "s"))];
  external llvmI32 "strcmp" [(llvmStringPointer, AST.Name (fromString "s")),
                                         (llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "jgets" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                     , (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32 "test" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "post" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "get" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_string" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "is_json_string" [ (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmStringPointer "tostring" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_double" [(llvmDouble, AST.Name (fromString "s"))];
  external llvmI32Pointer "is_json_double" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmDouble "get_json_double" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_array" [ (llvmI32PointerPointer, AST.Name (fromString "s"))
                                       , (llvmI32, (fromString "s"))];
  external llvmI32Pointer "create_arr_iter" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "arr_next_elem" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                          , (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "get_json_from_array" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                                , (llvmI32, (fromString "s"))];
  external llvmI32Pointer "json_bool" [(llvmI32, AST.Name (fromString"s"))];

externs = Map.fromList [
      ("log", "puts"),
      ("jn", "json"),
      ("clientPost", "post"),
      ("clientGet", "get"),
      ("jnum", "json_double"),
      ("getdoub", "get_json_double"),
      ("tostring", "tostring"),
      ("getfst", "create_arr_iter"),
      ("getnext", "arr_next_elem"),
      ("scmp", "strcmp"),
      ("floor", "floor"),
      ("isString", "is_json_string"),
      ("isNum", "is_json_double"),
      ("jbool", "json_bool")
  ]

extern2args = Map.fromList [
      ("gets", "jgets"),
      ("geta", "get_json_from_array")
  ]

boolOperators = Map.fromList [
    (Or, error "unimplemented operator")
  , (And, error "unimplemented operator")
  ]

numOperators = Map.fromList [
    (Plus, fadd)
  , (Minus, fsub)
  , (Multiply, fmul)
  , (Divide, fdiv)
  , (EQ, error "unimplemented operator")
  , (LEQ, error "unimplemented operator")
  , (GEQ, error "unimplemented operator")
  , (LT, error "unimplemented operator")
  , (GT, error "unimplemented operator")
  ]

opFns = Map.empty

buildLLVM :: Program -> LLVM ()
buildLLVM p = do
  mapM_ constantLLVM (constants p)
  let fns = fnDeclarations p
  mapM_ functionLLVM fns
  functionLLVMMain fns

constantLLVM :: (ValName, Term) -> LLVM ()
constantLLVM (name, term) = do
  -- looks like constants with GlobalVariable won't work, since we need to execute code to use JSON interop
  -- maybe we could declare globals as initially null, then generate code to change them
  error "constants unimplemented"

toSig :: String -> [(AST.Type, AST.Name)]
toSig x = [(llvmI32Pointer, AST.Name (fromString x))]

mainSig :: [(AST.Type, AST.Name)]
mainSig = [ (llvmI32Pointer, AST.Name (fromString "argc"))
          , (llvmPointerStringfPointer, AST.Name (fromString "argv"))]

functionLLVMMain :: [(FnName, Function)] -> LLVM ()
functionLLVMMain fns = do
  define llvmRetType "main" mainSig llvmBody
  where llvmRetType = llvmI32
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          let fnNames = map fst . filter (not . helper . snd) $ fns
          argv1 <- argvAt 1
          argv2 <- argvAt 2
          mapM_ (\f -> createEndpointCheck f argv1 argv2) fnNames
          ret $ Just (cons $ AST.Int 32 0)

createEndpointCheck :: String -> AST.Operand -> AST.Operand -> Codegen AST.Name
createEndpointCheck fnName cmdRef arg = do
  compare <- rawStringLLVM fnName
  compStrRes <- llvmCallExt2 cmdRef compare "strcmp"
  let equal = AST.ICmp Intypoo.EQ compStrRes (cons $ AST.Int 32 0) []
  refEq <- instr $ equal
  iff <- addBlock fnName
  continue <- addBlock "continue"
  cbr refEq iff continue

  setBlock iff
  args <- functionCallLLVM "json_string" arg
  functionCallLLVM fnName args
  br continue
  iff <- getBlock

  setBlock continue

argvAt:: Integer -> Codegen AST.Operand
argvAt idx = do
  let argv = local (AST.Name (fromString "argv"))
  let ptr = AST.GetElementPtr True argv [cons $ AST.Int 32 idx] []
  ref <- instr $ ptr
  let load = AST.Load False ref Nothing 1 []
  op <- instr $ load
  return op

--fix return type
functionLLVM :: (FnName, Function) -> LLVM ()
functionLLVM (name, (Function {..})) = define llvmRetType name fnargs llvmBody
  where llvmRetType = llvmI32Pointer
        fnargs = toSig arg
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          let argptr = local (AST.Name (fromString arg))
          l <- alloca llvmI32Pointer
          store l argptr
          assign arg l
          expressionBlockLLVM body >>= ret . Just

expressionBlockLLVM :: ExpressionBlock -> Codegen AST.Operand
expressionBlockLLVM exprs = last <$> mapM expressionLLVM exprs

expressionLLVM :: Expression -> Codegen AST.Operand
expressionLLVM (Unassigned term) = termLLVM term
expressionLLVM (Assignment name term) = do
  maybeVal <- getvar name
  ptr <- termLLVM term
  l <- case maybeVal of
    Nothing -> do
      l <- alloca llvmI32Pointer
      assign name l
      return l
    Just val -> return val
  store l ptr
  return ptr

scopedBlockLLVM :: ExpressionBlock -> Codegen AST.Operand
scopedBlockLLVM exprs = do
  symTable <- symtab <$> get
  res <- expressionBlockLLVM exprs
  modify $ \state -> state {symtab = symTable}
  return res

termLLVM :: Term -> Codegen AST.Operand
termLLVM (FunctionCall fname arg) = do
  op <- termLLVM arg
  functionCallLLVM fname op
termLLVM (Accessor tTerm indexTerm) = do
  t <- termLLVM tTerm
  index <- termLLVM indexTerm

   -- TODO check if t is array
   -- TODO check if index is num (or int?)

  index_double <- functionCallLLVM "getdoub" index
  index_int <- instr $ AST.FPToUI index_double llvmI32 []

  element <- call
               (externf (AST.Name (fromString "get_json_from_array")))
               [t, index_int]

  return element
termLLVM (OperatorTerm opp t1 t2) = do
  case Map.lookup opp numOperators of
    Just ap -> do
      evalt1 <- termLLVM t1
      double1 <- functionCallLLVM "getdoub" evalt1
      evalt2 <- termLLVM t2
      double2 <- functionCallLLVM "getdoub" evalt2
      result <- ap double1 double2
      functionCallLLVM "jnum" result
    Nothing -> case Map.lookup opp boolOperators of
      Just oper -> do
        error "boolean operators not implemented"
      Nothing -> error $ "unimplemented operator " ++ show opp

termLLVM (IfThenElse bool tr fal) = do
  iff <- addBlock "iff"
  ielse <- addBlock "ielse"
  iexit <- addBlock "iexit"
  bool <- termLLVM bool
  boolasdoub <- functionCallLLVM "getdoub" bool
  branchval <- fcmp Floatypoo.ONE (cons $ AST.Float (Fl.Double 0.0)) boolasdoub
  cbr branchval iff ielse

  setBlock iff
  tval <- scopedBlockLLVM tr
  br iexit
  iff <- getBlock

  setBlock ielse
  fval <- scopedBlockLLVM fal
  br iexit
  ielse <- getBlock

  setBlock iexit
  phi llvmI32Pointer [(tval, iff), (fval, ielse)]

termLLVM (ForeachInDo var container body) = do
  loop <- addBlock "loop"
  exit <- addBlock "exit"

  l <- alloca llvmI32Pointer
  pcontainer <- termLLVM container
  firstel <- functionCallLLVM "getfst" pcontainer
  store l firstel
  assign var l
  ptrAsInt <- instr $ AST.PtrToInt firstel llvmI32 []
  test <- icmp Intypoo.NE (cons $ AST.Int 32 0) ptrAsInt
  cbr test loop exit

  setBlock loop
  scopedBlockLLVM body
  curr <- load l
  next <- llvmCallExt2 curr pcontainer "arr_next_elem"
  store l next
  ptrAsInt <- instr $ AST.PtrToInt next llvmI32 []
  test <- icmp Intypoo.NE (cons $ AST.Int 32 0) ptrAsInt
  cbr test loop exit

  setBlock exit
  return pcontainer

termLLVM (Literal prim) = primLLVM prim
termLLVM (Variable var) = do
  maybeVal <- getvar var
  case maybeVal of
    Nothing -> error $ "Local variable not in scope: " ++ show var
    Just val -> load val

primLLVM :: PrimValue -> Codegen AST.Operand
primLLVM (ArrVal arr) = do
  elemPtrs <- mapM termLLVM arr
  ptrArray <- buildPtrArray elemPtrs
  llvmCallJsonArr ptrArray (length elemPtrs)
primLLVM (ObjVal obj) = error "unimplemented: object literals"
primLLVM (NumVal num) = functionCallLLVM "jnum" (cons (AST.Float (Fl.Double num)))
primLLVM (StrVal s) = stringLLVM s
primLLVM (NullVal) = nullLLVM
primLLVM (TrueVal) = trueLLVM
primLLVM (FalseVal) = falseLLVM

nullLLVM :: Codegen AST.Operand
nullLLVM = error "need to build a null builder"

trueLLVM :: Codegen AST.Operand
trueLLVM = do
  tr <- (rawStringLLVM "true\0")
  functionCallLLVM "json_bool" (cons $AST.Int 32 (fromIntegral 1))

falseLLVM :: Codegen AST.Operand
falseLLVM = do
  functionCallLLVM "json_bool" (cons $AST.Int 32 (fromIntegral 0))

llvmCallJsonArr :: AST.Operand -> Int -> Codegen AST.Operand
llvmCallJsonArr elemPtrArray n = call (externf (AST.Name (fromString "json_array")))
  [elemPtrArray, (cons $ AST.Int 32 (fromIntegral n))]

functionCallLLVM :: String -> AST.Operand -> Codegen AST.Operand
functionCallLLVM fn arg = do
  case Map.lookup fn externs of
    Just fn2 -> do
      llvmCallExt arg fn2
    Nothing -> case Map.lookup fn extern2args of
      Just fn3 -> do
        llvmCallExt2args arg fn3
      Nothing -> llvmCallFunc fn arg

llvmCallExt :: AST.Operand -> String -> Codegen AST.Operand
llvmCallExt op func =
  if func == "puts" || func == "post"
  then do
    st <- functionCallLLVM "tostring" op
    call (externf (AST.Name (fromString func))) [st]
    return op
  else call (externf (AST.Name (fromString func))) [op]

llvmCallExt2 :: AST.Operand -> AST.Operand -> String -> Codegen AST.Operand
llvmCallExt2 op op2 func = call (externf (AST.Name (fromString func))) [op, op2]

llvmCallExt2args :: AST.Operand -> String -> Codegen AST.Operand
llvmCallExt2args op func = do
  op1 <- call (externf (AST.Name (fromString "get_json_from_array"))) [op, cons $ AST.Int 32 (fromIntegral 0)]
  op2 <- call (externf (AST.Name (fromString "get_json_from_array"))) [op, cons $ AST.Int 32 (fromIntegral 1)]
  if func == "get_json_from_array"
  then do
    idx <- functionCallLLVM "getdoub" op2
    let conv = AST.FPToUI idx llvmI32 []
    intidx <- instr $conv
    llvmCallExt2 op1 intidx func
  else llvmCallExt2 op1 op2 func

llvmCallFunc :: String -> AST.Operand -> Codegen AST.Operand
llvmCallFunc fnName op = call (externf (AST.Name (fromString fnName))) [op]

--llvmCallGetArrIter :: AST.Operand -> Codegen AST.Operand
--llvmCallGetArrIter arr
--llvmArrayToPointer :: AST.Constant -> AST.Constant
--llvmArrayToPointer arr = AST.GetElementPtr True arr [AST.Int 32 0]

llvmCharArrayType :: Int -> AST.Type
llvmCharArrayType n = AST.ArrayType (fromIntegral n :: Word64) llvmI8

buildPtrArray :: [AST.Operand] -> Codegen AST.Operand
buildPtrArray ptrs = do
  mem <- instr $
    AST.Alloca llvmI32Pointer (Just (cons (AST.Int 32 (fromIntegral (length ptrs))))) 0 []
  forM_ [0..(length ptrs)-1] $ \i -> do
    let tempptr = AST.GetElementPtr True mem [cons $ AST.Int 32 (fromIntegral i)] []
    tempmem <- instr $ tempptr
    instr $ AST.Store False tempmem (ptrs!!i) Nothing (fromIntegral 0) []
  return mem

stringToLLVMString :: String -> AST.Constant
stringToLLVMString s = AST.Array llvmI8 (map charToLLVMInt s ++ [AST.Int 8 0])

charToLLVMInt :: Char -> AST.Constant
charToLLVMInt = AST.Int 8 . fromIntegral . ord

rawStringLLVM :: String -> Codegen AST.Operand
rawStringLLVM s = do
  let ptr =
        AST.Alloca (llvmCharArrayType (1+length s)) (Just (cons (AST.Int 32 1))) 0 []
  op <- instr $ ptr
  let arrayS = stringToLLVMString s
  _ <- instr $ AST.Store False op (cons arrayS) Nothing 0 []
  let ref = AST.GetElementPtr True op [cons $ AST.Int 8 0, cons $ AST.Int 8 0] []
  op2 <- instr $ ref
  return op2

stringLLVM :: String -> Codegen AST.Operand
stringLLVM s = do
  op <- rawStringLLVM s
  functionCallLLVM "json_string" op

