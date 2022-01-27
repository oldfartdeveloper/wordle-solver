module Main where

import Prelude

import Data.Array (all, filter, mapWithIndex, nub)
import Data.Char.Utils (toCodePoint)
import Data.CodePoint.Unicode (isAsciiLower, isLetter)
import Data.Foldable (any, foldl)
import Data.String (joinWith, singleton)
import Data.String.CodePoints (CodePoint, codePointAt, codePointFromChar, fromCodePointArray, indexOf, toCodePointArray)
import Data.String.CodeUnits (contains, toCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Data.String.Utils (lines)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile, writeTextFile)

unknownPosition :: Char
unknownPosition = '.'

lettersInPosition :: String
lettersInPosition = ".U..."

lettersInPositionInternal :: Array (Tuple Int CodePoint)
lettersInPositionInternal = mapWithIndex convert $ toCharArray lettersInPosition
  where
    convert :: Int -> Char -> Tuple Int CodePoint
    convert offset candidate = Tuple offset (codePointFromChar candidate)

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
-- and whose position is not known.
letters :: Array CodePoint
letters =
  lettersUsed <> (filter (\t -> isLetter t) $ map snd lettersInPositionInternal)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words5.txt"
  log $ play $ lines str

play :: Array String -> String
play words =
  fromCodePointArray $ unusedLetters letters attempts

-- What are the letters that are not used in the answer?
unusedLetters :: Array CodePoint -> Array String -> Array CodePoint
unusedLetters used atts =
  filter onlyUnused $ nub $ toCodePointArray $ joinWith "" atts

onlyUnused :: CodePoint -> Boolean
onlyUnused cp =
  not $ contains (Pattern $ singleton cp) $ fromCodePointArray letters
