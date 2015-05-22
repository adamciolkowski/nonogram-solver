module Main where
import System.Environment
import System.IO

import Puzzle
import PuzzleSolver
import PuzzlePrinter

main = do args <- getArgs
          handle <- openFile (head args) ReadMode
          cols <- hGetLine handle
          rows <- hGetLine handle
          hClose handle
          printSolution (solvePuzzle (Puzzle (getHints cols) (getHints rows)))
          where
            getHints hs = (read hs)::[[Int]]
