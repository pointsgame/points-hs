module Main where

import FieldTests
import Test.Framework
import Test.Framework.Providers.HUnit

main :: IO ()
main =
  defaultMain
    [ testCase "simple surround" simpleSurround,
      testCase "surround empty territory" surroundEmptyTerritory,
      testCase "move priority" movePriority,
      testCase "move priority, big" movePriorityBig,
      testCase "onion surroundings" onionSurroundings,
      testCase "deep onion surroundings" deepOnionSurroundings,
      testCase "apply 'control' surrounding in same turn" applyControlSurroundingInSameTurn,
      testCase "double surround" doubleSurround,
      testCase "double surround with empty part" doubleSurroundWithEmptyPart,
      testCase "should not leave empty inside" shouldNotLeaveEmptyInside,
      testCase "surround in opposite turn" surroundInOppositeTurn,
      testCase "partly surround in opposite turn" partlySurroundInOppositeTurn,
      testCase "a hole inside a surrounding" holeInsideSurrounding,
      testCase "a hole inside a surrounding, after opposite turn surrounding" holeInsideSurroundingAfterOppositeTurnSurrounding,
      testCase "surrounding does not expand" surroundingDoesNotExpand,
      testCase "2 surroundings with common border" twoSurroundingsWithCommonBorder,
      testCase "2 surroundings with common dot" twoSurroundingsWithCommonDot,
      testCase "3 surroundings with common borders" threeSurroundingsWithCommonBorders,
      testCase "2 surroundings with common dot, one borderline empty place" twoSurroundingsWithCommonDotOneBorderlineEmptyPlace,
      testCase "ambiguous surrounding 1" ambiguousSurrounding1,
      testCase "ambiguous surrounding 2" ambiguousSurrounding2
    ]
