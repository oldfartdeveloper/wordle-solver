module Main where

import Prelude

import Data.Array (all, filter, nub)
import Data.CodePoint.Unicode (isAsciiLower, isLetter)
import Data.Foldable (foldl)
import Data.String (joinWith, toCodePointArray)
import Data.String.CodePoints (CodePoint, fromCodePointArray, toCodePointArray)
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile, writeTextFile)

unknownPosition :: Char
unknownPosition = '.'

lettersInPosition :: String
lettersInPosition = ".U..."

-- Only need 5 attempts
attempts :: Array String
attempts =
  [ "ATONE"
  , "BUILD"
  , "CRYPT"
  , ""
  , ""
  ]

-- Letters used but whose position in the answer isn't yet known.
lettersUsed :: Array CodePoint
lettersUsed = toCodePointArray "AR"

-- Calculated letters used, both whose position is known
-- and whose position are not known.
letters :: Array CodePoint
letters =
  lettersUsed <> (filter (\c -> isLetter c) $ toCodePointArray lettersInPosition)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words5.txt"
  log $ play $ lines str

play :: Array String -> String
play words =
  fromCodePointArray letters

-- What are the letters that are not used in the answer?
unusedLetters :: Array CodePoint -> Array String -> Array CodePoint
unusedLetters used atts =
  filter onlyUnused $ nub $ toCodePointArray $ joinWith "" atts

onlyUnused :: CodePoint -> Boolean
onlyUnused cp =
  false

