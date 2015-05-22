module PuzzleSolver where
import System.IO
import Data.List
import Data.Maybe

import Puzzle

solvePuzzle :: Puzzle -> Solution
solvePuzzle p = fromJust (find (\s -> isValidSolution s p) (allPossibleSolutions p))

allPossibleSolutions :: Puzzle -> [Solution]
allPossibleSolutions (Puzzle rs cs) = map (Solution) (possibleSolutions rs (length rs) (length cs))

possibleSolutions :: [[Int]] -> Int -> Int -> [[[Cell]]]
possibleSolutions _ 0 len = [[]]
possibleSolutions (h:hs) x len = [rs : rss | rs <- possibleRowsForHint h len, rss <- possibleSolutions hs (x - 1) len]

possibleRows :: Int -> [[Cell]]
possibleRows 0 = [[]]
possibleRows len = [c : rs | c <- [Filled, Blank], rs <- possibleRows (len - 1)]

possibleRowsForHint :: [Int] -> Int -> [[Cell]]
possibleRowsForHint hs len = permuteWithoutDuplicates ((replicate noFilled Filled) ++ (replicate (len - noFilled) Blank))
                             where
                               noFilled = sum hs

permuteWithoutDuplicates xs = nub (permutations xs)

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
