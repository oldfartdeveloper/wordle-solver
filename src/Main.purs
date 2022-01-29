module Main where

import Prelude

import Data.Array (all, filter, index, mapWithIndex, nub)
import Data.Char.Utils (toCodePoint)
import Data.CodePoint.Unicode (isAsciiLower, isLetter)
import Data.Foldable (any, elem, foldl)
import Data.Maybe (Maybe, fromJust)
import Data.String (joinWith, null, singleton, trim)
import Data.String.CodePoints (CodePoint, codePointAt, codePointFromChar, fromCodePointArray, indexOf, toCodePointArray)
import Data.String.CodeUnits (contains, toCharArray)
import Data.String.Pattern (Pattern(Pattern))
import Data.String.Utils (lines)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile, writeTextFile)
import Partial.Unsafe (unsafePartial)

noLetterInThisPosition :: CodePoint
noLetterInThisPosition = codePointFromChar '.'

expectedLettersInPosition :: String
expectedLettersInPosition = "....."

expectedLettersInPositionT :: Array (Tuple Int CodePoint)
expectedLettersInPositionT = lettersWithPosition expectedLettersInPosition

lettersWithPosition :: String -> Array (Tuple Int CodePoint)
lettersWithPosition word =
  mapWithIndex convert $ toCharArray word
  where
    convert :: Int -> Char -> Tuple Int CodePoint
    convert offset candidate = Tuple offset (codePointFromChar candidate)

-- Only need 5 attempts
attempts :: Array String
attempts =
  filter (not null) $ map trim $ lines
    """
    ATONE
    BUILD
    CRYPT
    """

-- Letters used but whose position in the answer isn't yet known.
lettersUsed :: Array CodePoint
lettersUsed = toCodePointArray "EPRY"

-- Calculated letters used, both whose position is known
-- and whose position is not known.
letters :: Array CodePoint
letters =
  lettersUsed <>
    (filter (\t -> isLetter t) $ map snd $ expectedLettersInPositionT)

main :: Effect Unit
main = do
  str <- readTextFile ASCII "doc/words5.txt"
  log $ play $ lines str

-- FIXIT: screen out words that have any letter matching the position
-- of their used (but not same position).
play :: Array String -> String
play words =
  joinWith "\n" $
    let
      noUnused =
        filter (\word ->
                  not $ hasAnyUnused $ toCodePointArray word) words
      noUnusedAndHasAllUsed =
        filter (\word -> includesAllUsed $ toCodePointArray word) noUnused
    in
      if expectedLettersInPosition == "....."
      then {- noUnusedAndHasAllUsed -} noUnusedAndHasAllUsed
      else filter (\word -> hasAllInPosition word) noUnusedAndHasAllUsed
  -- in
    -- FIXIT!: Is not requiring unknown position letters in filter.
    -- joinWith "\n" $ filter (\word -> hasAllInPosition word) haveUsed
    -- haveUsed = fromCodePointArray $ unusedLetters lettersUsed attempts

haveOnlyUsed :: Array CodePoint -> Boolean
haveOnlyUsed cps = not $ hasAnyUnused cps

includesAllUsed :: Array CodePoint -> Boolean
includesAllUsed cps = all (\letter -> elem letter cps) letters

hasAnyUnused :: Array CodePoint -> Boolean
hasAnyUnused cps =
  any (\cp -> elem cp $ unusedLetters lettersUsed attempts) cps

hasAllInPosition :: String -> Boolean
hasAllInPosition cps =
    all (\cpT -> verifyCharInPosition cpT) $ lettersWithPosition cps

verifyCharInPosition :: Tuple Int CodePoint -> Boolean
verifyCharInPosition actualCpT =
  let
    expectedCpT = unsafeJust $ index expectedLettersInPositionT $ fst actualCpT
  in
  (noLetterInThisPosition == snd expectedCpT) ||
        (lettersMatch actualCpT expectedLettersInPositionT)

lettersMatch :: Tuple Int CodePoint -> Array (Tuple Int CodePoint) -> Boolean
lettersMatch actualChar expectedChars =
  actualChar == unsafeJust (index expectedChars $ fst actualChar)

-- What are the letters that are not used in the answer?
unusedLetters :: Array CodePoint -> Array String -> Array CodePoint
unusedLetters used atts =
  filter onlyUnused $ nub $ toCodePointArray $ joinWith "" atts

onlyUnused :: CodePoint -> Boolean
onlyUnused cp =
  not $ elem cp letters

convert :: Int -> Char -> Tuple Int CodePoint
convert offset candidate = Tuple offset (codePointFromChar candidate)

unsafeJust :: forall a. Maybe a -> a
unsafeJust x = unsafePartial $ fromJust x
