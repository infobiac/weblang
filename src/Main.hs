module Main (main) where

import Lexer (tokenize)
import Parser (parse)
import AST

main :: IO ()
main = do
  s <- getContents
  let tokens = tokenize s
  putStrLn $ "Tokens: "
  mapM_ (\t -> putStrLn ("  " ++ show t)) tokens
  putStrLn ""
  let parsed = parse tokens
  putStrLn $ "AST:"
  putStrLn $ "  " ++ show parsed
  putStrLn ""
  putStrLn $ "Hello world interpreter:"
  putStrLn ""
  simpleRunProgram parsed

simpleRunProgram :: Program -> IO ()
simpleRunProgram (Program fns) = mapM_ (mapM_ simpleRunExpression . body) (lookup "main" fns)

simpleRunExpression :: (Int, Expression) -> IO ()
simpleRunExpression (2, Unassigned term) = simpleRunTerm term
simpleRunExpression (2, Assignment _ term) = simpleRunTerm term
simpleRunExpression _ = return ()

simpleRunTerm :: Term -> IO ()
simpleRunTerm (FunctionCall "log" (Literal (StrVal str))) = putStrLn str
simpleRunTerm (Literal (ArrVal ts)) = mapM_ simpleRunTerm ts
simpleRunTerm (Literal (ObjVal ts)) = mapM_ simpleRunTerm ts
simpleRunTerm _ = return ()
