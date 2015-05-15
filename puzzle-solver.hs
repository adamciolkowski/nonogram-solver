import System.IO

readPuzzle path = do handle <- openFile path ReadMode
                     cols <- hGetLine handle
                     rows <- hGetLine handle
                     hClose handle
                     printSolution (solvePuzzle (Puzzle (parse cols) (parse rows)))
                     return ()
					 where
                       parse s = (read s)::[[Int]]

data Puzzle = Puzzle [[Int]] [[Int]] deriving Show
data Cell = Filled | Blank deriving Show
data Solution = Solution [[Cell]] deriving Show

solvePuzzle :: Puzzle -> Solution
solvePuzzle (Puzzle cols rows) = Solution [[]]	-- TODO

printSolution :: Solution -> IO ()
printSolution s = do return () 	-- TODO
