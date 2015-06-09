module Main where
import Test.HUnit
import qualified PuzzleSolverTest

main = do
  runTestTT $ TestList $
      PuzzleSolverTest.tests
