{-# LANGUAGE RecordWildCards #-}
module LLVM where

import Prelude hiding (EQ, LEQ, GEQ, GT, LT)
import Program
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.Module as Module
import qualified LLVM.Internal.Context as Context
import qualified LLVM.AST.Constant as AST hiding (GetElementPtr, FCmp, ICmp, PtrToInt, FPToUI, ZExt)
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
  external llvmI32Pointer "json_from_string" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_object" [(llvmI32PointerPointer, AST.Name(fromString "s"))
                                     , (llvmI32, (fromString "s"))];
  external llvmI32Pointer "is_json_object" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "add_to_json_object" [(llvmI32Pointer, AST.Name (fromString "s"))
                                     , (llvmI32Pointer, AST.Name (fromString "s"))
                                     , (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32 "exit" [(llvmI32, AST.Name (fromString "s"))];
  external llvmI32 "puts" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32 "floor" [(llvmDouble, AST.Name (fromString "s"))];
  external llvmI32 "strcmp" [(llvmStringPointer, AST.Name (fromString "s")),
                                         (llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "jgets" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                     , (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32 "test" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "post" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "get" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_string" [(llvmStringPointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "is_json_string" [ (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmStringPointer "tostring" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_double" [(llvmDouble, AST.Name (fromString "s"))];
  external llvmI32Pointer "to_json_double" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "is_json_double" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmDouble "get_json_double" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_array" [ (llvmI32PointerPointer, AST.Name (fromString "s"))
                                       , (llvmI32, (fromString "s"))];
  external llvmI32Pointer "is_json_array" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "get_json_from_array" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                                , (llvmI32, (fromString "s"))];
  external llvmI32Pointer "push_to_json_array" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                                , (llvmI32Pointer, (fromString "s"))];
  external llvmI32Pointer "replace_json_array_element" [(llvmI32Pointer, AST.Name (fromString "s"))
                                                , (llvmI32Pointer, (fromString "s"))
                                                , (llvmI32Pointer, (fromString "s"))];
  external llvmI32Pointer "create_arr_iter" [(llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "arr_next_elem" [ (llvmI32Pointer, AST.Name (fromString "s"))
                                          , (llvmI32Pointer, AST.Name (fromString "s"))];
  external llvmI32Pointer "json_bool" [(llvmI32, AST.Name (fromString"s"))];
  external llvmI32Pointer "is_json_bool" [(llvmI32Pointer, AST.Name (fromString "s"))];

externs = Map.fromList [
      ("log", "puts"),
      ("jn", "json_from_string"),
      ("isObj", "is_json_object"),
      ("clientPost", "post"),
      ("clientGet", "get"),
      ("jnum", "json_double"),
      ("toNum", "to_json_double"),
      ("getdoub", "get_json_double"),
      ("tostring", "tostring"),
      ("getfst", "create_arr_iter"),
      ("getnext", "arr_next_elem"),
      ("scmp", "strcmp"),
      ("floor", "floor"),
      ("isString", "is_json_string"),
      ("isNum", "is_json_double"),
      ("isArr", "is_json_array"),
      ("jbool", "json_bool"),
      ("isBool", "is_json_bool")
  ]

extern2args = Map.fromList [
      ("get", "jgets"),
      ("geta", "get_json_from_array"),
      ("push", "push_to_json_array")
  ]


extern3args = Map.fromList [
      ("addToObj", "add_to_json_object"),
      ("update", "replace_json_array_element")
  ]

boolOperators = Map.fromList [
    (Or, error "unimplemented operator")
  , (And, error "unimplemented operator")
  ]

eqOperators = Map.fromList [
    (EQ, fcmp Floatypoo.OEQ)
  , (LEQ, fcmp Floatypoo.OLE)
  , (GEQ, fcmp Floatypoo.OGE)
  , (LT, fcmp Floatypoo.OLT)
  , (GT, fcmp Floatypoo.OGT)
  ]

numOperators = Map.fromList [
    (Plus, fadd)
  , (Minus, fsub)
  , (Multiply, fmul)
  , (Divide, fdiv)
  , (Modulus, fmod)
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

argvAt :: Integer -> Codegen AST.Operand
argvAt idx = do
  let argv = local (AST.Name (fromString "argv"))
  let ptr = AST.GetElementPtr True argv [cons $ AST.Int 32 idx] []
  ref <- instr $ ptr
  let load = AST.Load False ref Nothing 1 []
  op <- instr $ load
  return op

functionLLVM :: (FnName, Function) -> LLVM ()
functionLLVM (name, (Function {..})) = define llvmRetType name fnargs llvmBody
  where llvmRetType = llvmI32Pointer
        fnargs = toSig arg
        llvmBody = createBlocks $ execCodegen $ do
          entry <- addBlock entryBlockName
          setBlock entry
          let argptr = local (AST.Name (fromString arg))

          typeAssertionLLVM ("Pre-condition not met in function " ++ name) inputType argptr

          l <- alloca llvmI32Pointer
          store l argptr
          assign arg l
          res <- expressionBlockLLVM body

          typeAssertionLLVM ("Post-condition not met in function " ++ name) outputType res

          ret (Just res)

typeAssertionLLVM :: String -> Type -> AST.Operand -> Codegen ()
typeAssertionLLVM msg (Type {..}) val = forM_ predicates $ \(var, predBlock) -> do

  withoutVar <- symtab <$> get
  l <- alloca llvmI32Pointer
  store l val
  assign var l
  res <- expressionBlockLLVM predBlock
  modify $ \state -> state {symtab = withoutVar}

  assertionLLVM msg res

assertionLLVM :: String -> AST.Operand -> Codegen ()
assertionLLVM message res = do
  failureBlock <- addBlock "type-assertion-failed"
  exitBlock <- addBlock "iexit"

  boolasdoub <- functionCallLLVM "getdoub" res
  branchval <- fcmp Floatypoo.ONE (cons $ AST.Float (Fl.Double 0.0)) boolasdoub
  cbr branchval exitBlock failureBlock

  setBlock failureBlock
  messageString <- rawStringLLVM message
  call (externf (AST.Name (fromString "puts"))) [messageString]
  call (externf (AST.Name (fromString "exit"))) [cons $ AST.Int 32 1]
  br exitBlock
  ielse <- getBlock

  setBlock exitBlock
  return ()

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

  element <- call
               (externf (AST.Name (fromString "jgets")))
               [t, index]

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
    Nothing -> case Map.lookup opp eqOperators of
      Just ap -> do
        evalt1 <- termLLVM t1
        double1 <- functionCallLLVM "getdoub" evalt1
        evalt2 <- termLLVM t2
        double2 <- functionCallLLVM "getdoub" evalt2
        result <- ap double1 double2
        int32 <- instr $ AST.ZExt result llvmI32 []
        functionCallLLVM "json_bool" int32
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
primLLVM (ObjVal obj) = do
  elemPtrs <- mapM termLLVM obj
  ptrArray <- buildObjPtrArray (Map.toList elemPtrs)
  llvmCallJsonObj ptrArray (length elemPtrs)
primLLVM (NumVal num) = functionCallLLVM "jnum" (cons (AST.Float (Fl.Double num)))
primLLVM (StrVal s) = stringLLVM s
primLLVM (NullVal) = nullLLVM
primLLVM (TrueVal) = trueLLVM
primLLVM (FalseVal) = falseLLVM

nullLLVM :: Codegen AST.Operand
nullLLVM = error "need to build a null builder"

trueLLVM :: Codegen AST.Operand
trueLLVM = do
  functionCallLLVM "json_bool" (cons $AST.Int 32 (fromIntegral 1))

falseLLVM :: Codegen AST.Operand
falseLLVM = do
  functionCallLLVM "json_bool" (cons $AST.Int 32 (fromIntegral 0))

llvmCallJsonArr :: AST.Operand -> Int -> Codegen AST.Operand
llvmCallJsonArr elemPtrArray n = call (externf (AST.Name (fromString "json_array")))
  [elemPtrArray, (cons $ AST.Int 32 (fromIntegral n))]

llvmCallJsonObj :: AST.Operand -> Int -> Codegen AST.Operand
llvmCallJsonObj elemPtrArray n = call (externf (AST.Name (fromString "json_object")))
  [elemPtrArray, (cons $ AST.Int 32 (fromIntegral n))]

functionCallLLVM :: String -> AST.Operand -> Codegen AST.Operand
functionCallLLVM fn arg = do
  case Map.lookup fn externs of
    Just fn2 -> do
      llvmCallExt arg fn2
    Nothing -> case Map.lookup fn extern2args of
      Just fn3 -> do
        llvmCallExt2args arg fn3
      Nothing -> case Map.lookup fn extern3args of
          Just fn4 -> do
            llvmCallExt3args arg fn4
          Nothing -> llvmCallFunc fn arg

llvmCallExt :: AST.Operand -> String -> Codegen AST.Operand
llvmCallExt op func =
  if func == "puts"
  then do
    st <- functionCallLLVM "tostring" op
    call (externf (AST.Name (fromString func))) [st]
    return op
  else if func == "get" || func =="post"
  then do
    res <- call (externf (AST.Name (fromString func))) [op]
    ret <- functionCallLLVM "jn" res
    return ret
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

llvmCallExt3args :: AST.Operand -> String -> Codegen AST.Operand
llvmCallExt3args op func = do
  op1 <- call (externf (AST.Name (fromString "get_json_from_array"))) [op, cons $AST.Int 32 (fromIntegral 0)]
  op2 <- call (externf (AST.Name (fromString "get_json_from_array"))) [op, cons $AST.Int 32 (fromIntegral 1)]
  op3 <- call (externf (AST.Name (fromString "get_json_from_array"))) [op, cons $AST.Int 32 (fromIntegral 2)]
  call (externf (AST.Name (fromString func))) [op1, op2, op3]

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

buildObjPtrArray :: [(String, AST.Operand)] -> Codegen AST.Operand
buildObjPtrArray ptrs = do
  mem <- instr $
    AST.Alloca llvmI32Pointer (Just (cons (AST.Int 32 (fromIntegral (2*(length ptrs)))))) 0 []
  forM_ [0..(length ptrs)-1] $ \i -> do
    let tempptr1 = AST.GetElementPtr True mem [cons $ AST.Int 32 (2*(fromIntegral i))] []
    tempmem1 <- instr $ tempptr1
    op <- stringLLVM (fst (ptrs!!i))
    instr $AST.Store False tempmem1 op Nothing (fromIntegral 0) []
    let tempptr2 = AST.GetElementPtr True mem [cons $ AST.Int 32 ((2*(fromIntegral i))+1)] []
    tempmem2 <- instr $ tempptr2
    instr $AST.Store False tempmem2 (snd (ptrs!!i)) Nothing (fromIntegral 0) []
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

