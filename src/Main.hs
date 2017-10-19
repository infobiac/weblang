module Main (main) where

import Lexer

main = do
  s <- getContents
  mapM_ print (alexScanTokens s)
