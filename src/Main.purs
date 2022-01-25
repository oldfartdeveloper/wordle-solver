module Main where

import Prelude

import Data.Array (all, filter)
import Data.CodePoint.Unicode (isAsciiLower)
import Data.String (joinWith, length)
import Data.String.CodePoints (toCodePointArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile, writeTextFile)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words.txt"
  writeTextFile ASCII "doc/words5.txt" $
    joinWith "\n" (filter only5CharLowerCase (lines str))

only5CharLowerCase :: String -> Boolean
only5CharLowerCase word =
  (5 == length word) && (all isAsciiLower $ toCodePointArray word)
