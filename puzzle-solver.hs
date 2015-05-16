import System.IO
import System.Environment

main = do args <- getArgs
          handle <- openFile (head args) ReadMode
          cols <- hGetLine handle
          rows <- hGetLine handle
          hClose handle
          printSolution (solvePuzzle (Puzzle (parse cols) (parse rows)))
          where
            parse s = (read s)::[[Int]]

data Puzzle = Puzzle [[Int]] [[Int]] deriving Show
data Cell = Filled | Blank deriving Show
data Solution = Solution [[Cell]] deriving Show

solvePuzzle :: Puzzle -> Solution
solvePuzzle (Puzzle cols rows) = Solution [[]]	-- TODO

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
