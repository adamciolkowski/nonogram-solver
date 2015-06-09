module PuzzleSolverTest where
import Test.HUnit
import Puzzle
import PuzzleSolver

tests =
  [
   TestCase (assertEqual "hints: [0], length: 3" [[Blank, Blank, Blank]] (rowsForHint [0] 3)),
   TestCase (assertEqual "hints: [3], length: 3" [[Filled, Filled, Filled]] (rowsForHint [3] 3)),
   TestCase (assertEqual "hints: [1], length: 3" [[Filled, Blank, Blank],
                                                  [Blank, Filled, Blank],
                                                  [Blank, Blank, Filled]] (rowsForHint [1] 3)),
   TestCase (assertEqual "hints: [1, 1], length: 3" [[Filled, Blank, Filled]] (rowsForHint [1, 1] 3)),
   TestCase (assertEqual "hints: [2, 1], length: 5" [[Filled, Filled, Blank, Filled, Blank],
                                                     [Filled, Filled, Blank, Blank, Filled],
                                                     [Blank, Filled, Filled, Blank, Filled]] (rowsForHint [2, 1] 5))
  ]

