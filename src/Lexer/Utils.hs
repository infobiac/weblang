module Lexer.Utils where

import Data.String.Utils
import Lexer.Types

normalizeNewlines :: [LexToken] -> [LexToken]
normalizeNewlines = reduceNewlines . addStartingNewline

addStartingNewline :: [LexToken] -> [LexToken]
addStartingNewline = (NewlineToken :)

reduceNewlines :: [LexToken] -> [LexToken]
reduceNewlines (t1:t2:ts) = if isNewline t1 && isNewline t2
                            then reduceNewlines (t2:ts)
                            else t1:reduceNewlines (t2:ts)
reduceNewlines [t] = if isNewline t
                     then []
                     else [t]
reduceNewlines ts = ts

isNewline NewlineToken = True
isNewline (IndentToken _) = True
isNewline _ = False

parseQuoted :: String -> String
parseQuoted = replace "\\\"" "\"" . tail . init
