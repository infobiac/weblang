module Main where

import Parser
import Codegen
import Emit

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import LLVM.Module
import LLVM.Internal.Context
import qualified LLVM.AST as AST

initModule :: AST.Module
initModule = emptyModule "my cool jit"

process :: AST.Module -> String -> IO (Maybe AST.Module)
process modo source = do
  let res = parseToplevel source
  case res of
    Left err -> print err >> return Nothing
    Right ex -> do
      ast <- codegen modo ex
      return $ Just ast

processFile :: String -> IO (Maybe AST.Module)
processFile fname = readFile fname >>= process initModule

repl :: IO ()
repl = runInputT defaultSettings (loop initModule)
  where
  loop mod = do
    minput <- getInputLine "ready> "
    case minput of
      Nothing -> outputStrLn "Goodbye."
      Just input -> do
        modn <- liftIO $ process mod input
        case modn of
          Just modn -> loop modn
          Nothing -> loop mod

writeAssembly :: FilePath -> AST.Module -> IO ()
writeAssembly fp m =
  withContext (\context ->
                  withModuleFromAST context m (\m' ->
                                                  writeLLVMAssemblyToFile (File fp) m'))

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> repl
    [fname, llname] -> processFile fname >>= mapM_ (writeAssembly llname)
    _ -> repl
