module Main where

import Prelude

import Data.Array (all, filter)
import Data.CodePoint.Unicode (isAsciiLower, isLetter)
import Data.Foldable (foldl)
import Data.String (joinWith)
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
lettersUsed = "AR"

-- Calculated letters used, both whose position is known
-- and whose position are not known.
letters :: String
letters =
  let
    cps :: Array CodePoint
    cps = filter (\c -> isLetter c) $ toCodePointArray lettersInPosition
  in
  lettersUsed <> (fromCodePointArray cps)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words5.txt"
  log $ play $ lines str

play :: Array String -> String
play words =
  letters

-- What are the letters that are not used in the answer?
unusedLetters :: String -> Array String -> String
unusedLetters used attempts =
  foldl insertUnique "" $ toCharArray $ joinWith "" attempts

insertUnique :: String -> Char -> String
insertUnique acc char =
  ""
