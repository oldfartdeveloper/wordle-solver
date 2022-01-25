module Main where

import Prelude

import Data.Array (all, filter, length) as Ar
import Data.CodePoint.Unicode(isAsciiLower)
import Data.String.CodePoints(toCodePointArray)
import Data.String.Utils (length, lines) as Ut
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile, writeTextFile)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words.txt"
  log $ show $ Ar.length (Ar.filter only5CharLowerCase (Ut.lines str))

only5CharLowerCase :: String -> Boolean
only5CharLowerCase word =
  (5 == Ut.length word) && (Ar.all isAsciiLower $ toCodePointArray word)

-- wordle :: String -> Array String
-- wordle str = []
