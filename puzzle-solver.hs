import Data.List
import Data.Maybe
import System.Environment
import System.IO

main = do args <- getArgs
          handle <- openFile (head args) ReadMode
          cols <- hGetLine handle
          rows <- hGetLine handle
          hClose handle
          printSolution (solvePuzzle (Puzzle (parse cols) (parse rows)))
          where
            parse s = (read s)::[[Int]]

data Puzzle = Puzzle [[Int]] [[Int]] deriving Show

colCount (Puzzle c _) = length c
rowCount (Puzzle _ r) = length r

data Cell = Filled | Blank deriving (Eq, Show)
data Solution = Solution [[Cell]] deriving Show

solvePuzzle :: Puzzle -> Solution
solvePuzzle p = fromJust (find (\s -> isValidSolution s p) (allPossibleSolutions p))

allPossibleSolutions :: Puzzle -> [Solution]
allPossibleSolutions p = map (\rs -> Solution rs) (possibleSolutions (colCount p) (rowCount p))

possibleSolutions :: Int -> Int -> [[[Cell]]]
possibleSolutions 0 len = [[]]
possibleSolutions x len = [rs : rss | rs <- possibleRows len, rss <- possibleSolutions (x - 1) len]

possibleRows :: Int -> [[Cell]]
possibleRows 0 = [[]]
possibleRows len = [c : rs | c <- [Filled, Blank], rs <- possibleRows (len - 1)]

isValidSolution :: Solution -> Puzzle -> Bool
isValidSolution (Solution rs) (Puzzle rows cols) = (areRowsValid rows rs) && (areRowsValid cols (transpose rs))

areRowsValid :: [[Int]] -> [[Cell]] -> Bool
areRowsValid [] [] = True
areRowsValid (h:hs) (r:rs) = if (isRowValid h r) then areRowsValid hs rs
                             else False

isRowValid :: [Int] -> [Cell] -> Bool
isRowValid hs cs = hs == hintsFor cs

hintsFor :: [Cell] -> [Int]
hintsFor cs = getHints 0 cs
              where
                getHints :: Int -> [Cell] -> [Int]
                getHints 0 [] = []
                getHints i [] = [i]
                getHints i (c:cs) = if c == Blank then
                                        if i == 0 then getHints 0 cs
                                        else i : getHints 0 cs
                                    else getHints (i + 1) cs

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
