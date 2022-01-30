module Main where

import Prelude

import Data.Array (all, filter, index, mapWithIndex, nub)
import Data.CodePoint.Unicode (isLetter)
import Data.Foldable (any, elem)
import Data.Maybe (Maybe, fromJust)
import Data.String (joinWith, null, trim)
import Data.String.CodePoints
  ( CodePoint
  , codePointFromChar
  , toCodePointArray
  )
import Data.String.CodeUnits (toCharArray)
import Data.String.Utils (lines)
import Data.Tuple (Tuple(Tuple), fst, snd)
import Effect (Effect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(ASCII))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

noLetterInThisPosition :: CodePoint
noLetterInThisPosition = codePointFromChar '.'

expectedLettersInPosition :: String
expectedLettersInPosition = "...NT"

expectedLettersInPositionT :: Array (Tuple Int CodePoint)
expectedLettersInPositionT = lettersWithPosition expectedLettersInPosition

lettersWithPosition :: String -> Array (Tuple Int CodePoint)
lettersWithPosition word =
  mapWithIndex (\i c -> Tuple i $ codePointFromChar c) $ toCharArray word

-- Only need 5 attempts
attempts :: Array String
attempts =
  filter (not null) $ map trim $ lines
    """
    ATONE
    BUILD
    CRYPT
    FOUNT
    MOUNT
    """

-- Letters used but whose position in the answer isn't yet known.
-- The position specfied indicates that it specifically can't be that one.
lettersUsedWithPositions :: Array (Tuple Int CodePoint)
lettersUsedWithPositions =
  map adjustLetterUsed
    [ Tuple 2 'U'
    , Tuple 2 'T'
    , Tuple 3 'O'
    ]

-- Convert position from position to offset
-- and letter from char to code point
adjustLetterUsed :: Tuple Int Char -> Tuple Int CodePoint
adjustLetterUsed t =
  Tuple ((-1) + (fst t)) (codePointFromChar $ snd t)

lettersUsed :: Array CodePoint
lettersUsed =
  map (\t -> snd t) lettersUsedWithPositions

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

play :: Array String -> String
play words =
  joinWith "\n" $
    let
      noUnused =
        filter
          ( \word ->
              not $ hasAnyUnused $ toCodePointArray word
          )
          words
      noUnusedAndHasAllUsed =
        filter (\word -> includesAllUsed $ lettersWithPosition word) noUnused
    in
      if expectedLettersInPosition == "....."
      then noUnusedAndHasAllUsed
      else filter (\word -> hasAllInPosition word) noUnusedAndHasAllUsed

haveOnlyUsed :: Array CodePoint -> Boolean
haveOnlyUsed cps = not $ hasAnyUnused cps

includesAllUsed :: Array (Tuple Int CodePoint) -> Boolean
includesAllUsed tcps =
  all (\lp -> elem (snd lp) (map snd tcps) && (not $ elem lp tcps)) lettersUsedWithPositions

hasAnyUnused :: Array CodePoint -> Boolean
hasAnyUnused cps =
  any (\cp -> elem cp $ unusedLetters attempts) cps

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
unusedLetters :: Array String -> Array CodePoint
unusedLetters atts =
  filter onlyUnused $ nub $ toCodePointArray $ joinWith "" atts

onlyUnused :: CodePoint -> Boolean
onlyUnused cp =
  not $ elem cp letters

convert :: Int -> Char -> Tuple Int CodePoint
convert offset candidate = Tuple offset (codePointFromChar candidate)

unsafeJust :: forall a. Maybe a -> a
unsafeJust x = unsafePartial $ fromJust x
