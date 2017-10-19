module Lexer.Utils where

import Data.String.Utils

parseQuoted :: String -> String
parseQuoted = replace "\\\"" "\"" . tail . init
