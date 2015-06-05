module Main where
import System.Environment

import Puzzle
import PuzzleSolver
import PuzzlePrinter
import PuzzleReader

main = do args <- getArgs
          if (null args) then error "No path to file given"
          else do puzzle <- readPuzzle (head args)
                  printSolution (solvePuzzle puzzle)
