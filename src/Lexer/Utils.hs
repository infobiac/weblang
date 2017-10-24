module Lexer.Utils where

import Data.String.Utils
import Lexer.Types
import qualified Data.Set as Set
import Data.Set (Set)

normalizeNewlines :: [Pos LexToken] -> [Pos LexToken]
normalizeNewlines = newlineAbsorbers (Set.fromList [CommaToken, ColonToken, ArrowToken]) . reduceNewlines . addStartingNewline

addStartingNewline :: [Pos LexToken] -> [Pos LexToken]
addStartingNewline = (Pos 0 0 NewlineToken :)

reduceNewlines :: [Pos LexToken] -> [Pos LexToken]
reduceNewlines (t1:t2:ts) = if isNewline t1 && isNewline t2
                            then reduceNewlines (t2:ts)
                            else t1:reduceNewlines (t2:ts)
reduceNewlines [t] = if isNewline t
                     then []
                     else [t]
reduceNewlines ts = ts

newlineAbsorbers :: Set LexToken -> [Pos LexToken] -> [Pos LexToken]
newlineAbsorbers as (t1:t2:ts)
  | absorbs t1 && isNewline t2 = newlineAbsorbers as (t1:ts)
  | absorbs t2 && isNewline t1 = newlineAbsorbers as (t2:ts)
  | otherwise = t1:newlineAbsorbers as (t2:ts)
  where absorbs (Pos _ _ t) = t `Set.member` as
newlineAbsorbers _ ts = ts

isNewline (Pos _ _ NewlineToken) = True
isNewline (Pos _ _ (IndentToken _)) = True
isNewline _ = False

parseQuoted :: String -> String
parseQuoted = replace "\\\"" "\"" . tail . init
