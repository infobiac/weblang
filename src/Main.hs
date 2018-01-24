module Main (main) where

import Lexer (tokenize)
import Parser (parse)
import AST (includes, sourceAddress)
import Program
import LLVM
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Text.PrettyPrint.GenericPretty
import System.Environment
import System.Exit
import Control.Monad
import Semantics

main :: IO ()
main = do
  args <- getArgs
  when (length args < 2) usage
  let inputFile:outputFile:_ = args

  asts <- pullFileASTs inputFile
  let ir = astToProgram $ mconcat asts

  putStrLn $ "IR:"
  pp ir

  when (not (checkProgram ir)) $ putStrLn "Program did not typecheck!"

  putStrLn $ "Writing LLVM assembly to " ++ outputFile
  writeModule outputFile . buildModule $ ir

fileAST :: FilePath -> IO AST
fileAST fp = parse . tokenize <$> readFile fp

pullFileASTs :: FilePath -> IO [AST]
pullFileASTs fp = evalStateT (pullFileASTs' fp) Set.empty

pullFileASTs' :: FilePath -> StateT (Set FilePath) IO [AST]
pullFileASTs' fp = do
  done <- get
  if fp `Set.member` done
    then return []
    else do put $ fp `Set.insert` done
            root <- lift $ fileAST fp
            let next = map sourceAddress (includes root)
            (root:) . concat <$> mapM pullFileASTs' next

usage :: IO ()
usage = do
  putStrLn "Usage:"
  pName <- getProgName
  putStrLn $ "\t" ++ pName ++ " InputFile OutputFile"
  exitFailure
