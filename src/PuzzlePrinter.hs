module PuzzlePrinter where
import System.IO
import Puzzle

printSolution :: Solution -> IO ()
printSolution (Solution rs) = do printBoard rs

printBoard [] = do putChar '\n'
printBoard (r:rs) = do printRow r
                       printBoard rs

printRow :: [Cell] -> IO ()
printRow [] = do putChar '\n'
printRow (c:cs) = do printCell c
                     printRow cs

printCell :: Cell -> IO ()
printCell Filled = do putChar 'X'
printCell Blank = do putChar ' '
