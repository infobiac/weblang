module Main (main) where

import Lexer (tokenize)
import Parser (parse)
--import AST
import Program
import LLVM
import Text.PrettyPrint.GenericPretty
import System.Environment
import Control.Monad
import Semantics

main :: IO ()
main = do
  s <- getContents
  let tokens = tokenize s
  putStrLn $ "Tokens: "
  mapM_ (\t -> putStrLn ("  " ++ show t)) tokens
  putStrLn ""
  let parsed = parse tokens
      ir = astToProgram parsed
  putStrLn $ "IR:"
  pp ir
  when (not (checkProgram ir)) $ putStrLn "Program did not typecheck!"

  args <- getArgs
  case args of
    [] -> putStrLn $ "No output file given, not writing compiling"
    [outFile] -> do putStrLn $ "Writing LLVM assembly to " ++ outFile
                    writeModule outFile . buildModule $ ir
