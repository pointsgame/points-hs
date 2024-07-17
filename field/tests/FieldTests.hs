module FieldTests where

import Control.Exception.Base qualified as Exception
import Data.Char
import Data.List
import Data.List.Split
import Data.Maybe
import Field
import Player
import Test.HUnit

constructField :: String -> Field
constructField image =
  let width' = length $ takeWhile (/= ' ') $ dropWhile (== ' ') image
      lines' = chunksOf width' $ filter (/= ' ') image
      assertion = Exception.assert $ all (\line -> length line == width') lines'
      height' = length lines'
      moves' =
        map (\(x, y, char) -> ((x, y), if isLower char then Red else Black)) $
          sortOn
            (\(_, _, char) -> (toLower char, isLower char))
            [ (x, y, char)
              | (y, line) <- zip [0 ..] lines',
                (x, char) <- zip [0 ..] line,
                toLower char /= toUpper char
            ]
   in assertion $ foldl' (\field (pos, player) -> fromJust $ putPoint pos player field) (emptyField width' height') moves'

simpleSurround :: Assertion
simpleSurround =
  let image =
        " .a. \
        \ cBa \
        \ .a. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4
        fmap (last . fst) (lastSurroundChain field) @?= Just (0, 1)

surroundEmptyTerritory :: Assertion
surroundEmptyTerritory =
  let image =
        " .a. \
        \ a.a \
        \ .a. "
      field = constructField image
   in do
        scoreRed field @?= 0
        scoreBlack field @?= 0
        lastSurroundChain field @?= Nothing
        isPuttingAllowed field (1, 1) @? "Putting in pos (1, 1) is not allowed."
        not (isPuttingAllowed field (0, 1)) @? "Putting in pos (0, 1) is allowed."
        not (isPuttingAllowed field (1, 0)) @? "Putting in pos (1, 0) is allowed."
        not (isPuttingAllowed field (1, 2)) @? "Putting in pos (1, 2) is allowed."
        not (isPuttingAllowed field (2, 1)) @? "Putting in pos (2, 1) is allowed."

movePriority :: Assertion
movePriority =
  let image =
        " .aB. \
        \ aCaB \
        \ .aB. "
      field = constructField image
   in do
        scoreRed field @?= 0
        scoreBlack field @?= 1
        fmap snd (lastSurroundChain field) @?= Just Black
        fmap (length . fst) (lastSurroundChain field) @?= Just 4
        fmap (last . fst) (lastSurroundChain field) @?= Just (1, 1)

movePriorityBig :: Assertion
movePriorityBig =
  let image =
        " .B.. \
        \ BaB. \
        \ aCaB \
        \ .aB. "
      field = constructField image
   in do
        scoreRed field @?= 0
        scoreBlack field @?= 2
        fmap snd (lastSurroundChain field) @?= Just Black
        fmap (length . fst) (lastSurroundChain field) @?= Just 6
        fmap (last . fst) (lastSurroundChain field) @?= Just (1, 2)

onionSurroundings :: Assertion
onionSurroundings =
  let image =
        " ...c... \
        \ ..cBc.. \
        \ .cBaBc. \
        \ ..cBc.. \
        \ ...c... "
      field = constructField image
   in do
        scoreRed field @?= 4
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 8

deepOnionSurroundings :: Assertion
deepOnionSurroundings =
  let image =
        " ...D... \
        \ ..DcD.. \
        \ .DcBcD. \
        \ DcBaBcD \
        \ .DcBcD. \
        \ ..DcD.. \
        \ ...D... "
      field = constructField image
   in do
        scoreRed field @?= 0
        scoreBlack field @?= 9
        fmap snd (lastSurroundChain field) @?= Just Black
        fmap (length . fst) (lastSurroundChain field) @?= Just 12

applyControlSurroundingInSameTurn :: Assertion
applyControlSurroundingInSameTurn =
  let image =
        " .a. \
        \ aBa \
        \ .a. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4

doubleSurround :: Assertion
doubleSurround =
  let image =
        " .a.a. \
        \ aAbAa \
        \ .a.a. "
      field = constructField image
   in do
        scoreRed field @?= 2
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 8
        fmap (last . fst) (lastSurroundChain field) @?= Just (2, 1)

doubleSurroundWithEmptyPart :: Assertion
doubleSurroundWithEmptyPart =
  let image =
        " .b.b.. \
        \ b.zAb. \
        \ .b.b.. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4
        fmap (last . fst) (lastSurroundChain field) @?= Just (2, 1)
        isPuttingAllowed field (1, 1) @? "Putting in pos (1, 1) is not allowed."
        not (isPuttingAllowed field (3, 1)) @? "Putting in pos (3, 1) is allowed."

shouldNotLeaveEmptyInside :: Assertion -- TODO: check with another last point
shouldNotLeaveEmptyInside =
  let image =
        " .aaaa.. \
        \ a....a. \
        \ a.b...a \
        \ .z.bC.a \
        \ a.b...a \
        \ a....a. \
        \ .aaaa.. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 18
        fmap (last . fst) (lastSurroundChain field) @?= Just (1, 3)
        not (isPuttingAllowed field (2, 3)) @? "Putting in pos (2, 3) is allowed."
        not (isPuttingAllowed field (2, 4)) @? "Putting in pos (2, 4) is allowed."
        not (isPuttingAllowed field (2, 2)) @? "Putting in pos (2, 2) is allowed."
        not (isPuttingAllowed field (1, 3)) @? "Putting in pos (1, 3) is allowed."
        not (isPuttingAllowed field (3, 3)) @? "Putting in pos (3, 3) is allowed."
        not (isPuttingAllowed field (1, 1)) @? "Putting in pos (1, 1) is allowed."

surroundInOppositeTurn :: Assertion
surroundInOppositeTurn =
  let image =
        " .a. \
        \ aBa \
        \ .a. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4

partlySurroundInOppositeTurn :: Assertion
partlySurroundInOppositeTurn =
  let image =
        " .a.. \
        \ aBa. \
        \ .a.a \
        \ ..a. "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4
        isPuttingAllowed field (2, 2) @? "Putting in pos (2, 2) is not allowed."

holeInsideSurrounding :: Assertion
holeInsideSurrounding =
  let image =
        " ....c.... \
        \ ...c.c... \
        \ ..c...c.. \
        \ .c..a..c. \
        \ c..a.a..c \
        \ .c..a..c. \
        \ ..c...c.. \
        \ ...cBc... \
        \ ....d.... "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 16
        fmap (last . fst) (lastSurroundChain field) @?= Just (4, 8)
        not (isPuttingAllowed field (4, 4)) @? "Putting in pos (4, 4) is allowed."
        not (isPuttingAllowed field (4, 1)) @? "Putting in pos (4, 1) is allowed."

holeInsideSurroundingAfterOppositeTurnSurrounding :: Assertion
holeInsideSurroundingAfterOppositeTurnSurrounding =
  let image =
        " ....b.... \
        \ ...b.b... \
        \ ..b...b.. \
        \ .b..a..b. \
        \ b..a.a..b \
        \ .b..a..b. \
        \ ..b...b.. \
        \ ...bCb... \
        \ ....b.... "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 16
        not (isPuttingAllowed field (4, 4)) @? "Putting in pos (4, 4) is allowed."
        not (isPuttingAllowed field (4, 1)) @? "Putting in pos (4, 1) is allowed."

surroundingDoesNotExpand :: Assertion
surroundingDoesNotExpand =
  let image =
        " ....a.... \
        \ ...a.a... \
        \ ..a.a.a.. \
        \ .a.a.a.a. \
        \ a.a.aBa.a \
        \ .a.a.a.a. \
        \ ..a.a.a.. \
        \ ...a.a... \
        \ ....a.... "
      field = constructField image
   in do
        scoreRed field @?= 1
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 4
        isPuttingAllowed field (6, 3) @? "Putting in pos (6, 3) is allowed."
        isPuttingAllowed field (4, 3) @? "Putting in pos (4, 3) is allowed."
        isPuttingAllowed field (4, 5) @? "Putting in pos (4, 5) is allowed."
        isPuttingAllowed field (6, 5) @? "Putting in pos (6, 5) is allowed."
        not (isPuttingAllowed field (5, 4)) @? "Putting in pos (5, 4) is allowed."

twoSurroundingsWithCommonBorder :: Assertion
twoSurroundingsWithCommonBorder =
  let image =
        " .a.. \
        \ aAa. \
        \ .bAa \
        \ ..a. "
      field = constructField image
   in do
        scoreRed field @?= 2
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 6
        fmap (last . fst) (lastSurroundChain field) @?= Just (1, 2)

twoSurroundingsWithCommonDot :: Assertion
twoSurroundingsWithCommonDot =
  let image =
        " .a.a. \
        \ aBcBa \
        \ .a.a. "
      field = constructField image
   in do
        scoreRed field @?= 2
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 8

threeSurroundingsWithCommonBorders :: Assertion
threeSurroundingsWithCommonBorders =
  let image =
        " ..a.. \
        \ .aAa. \
        \ ..bAa \
        \ .aAa. \
        \ ..a.. "
      field = constructField image
   in do
        scoreRed field @?= 3
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 8
        fmap (last . fst) (lastSurroundChain field) @?= Just (2, 2)

twoSurroundingsWithCommonDotOneBorderlineEmptyPlace :: Assertion
twoSurroundingsWithCommonDotOneBorderlineEmptyPlace =
  let image =
        " ..a.. \
        \ .aBa. \
        \ ..c.a \
        \ .aBa. \
        \ ..a.. "
      field = constructField image
   in do
        scoreRed field @?= 2
        scoreBlack field @?= 0
        fmap snd (lastSurroundChain field) @?= Just Red
        fmap (length . fst) (lastSurroundChain field) @?= Just 8
