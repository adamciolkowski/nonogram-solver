module PuzzleSolver where
import System.IO
import Data.List
import Data.Maybe

import Puzzle

solvePuzzle :: Puzzle -> Solution
solvePuzzle p = fromJust (find (\s -> isValidSolution s p) (possibleSolutions p))

possibleSolutions :: Puzzle -> [Solution]
possibleSolutions (Puzzle rs cs) = map (Solution) (validRowsSolutions rs (length rs) (length cs))

validRowsSolutions :: [[Int]] -> Int -> Int -> [[[Cell]]]
validRowsSolutions _ 0 len = [[]]
validRowsSolutions (h:hs) x len = [rs : rss | rs <- rowsForHint h len, rss <- validRowsSolutions hs (x - 1) len]

rowsForHint :: [Int] -> Int -> [[Cell]]
rowsForHint [0] len = [replicate len Blank]
rowsForHint [h] len | len == h = [replicate len Filled]
                    | len > h = [(replicate h Filled) ++ (replicate (len - h) Blank)] ++
                                [Blank : rs | rs <- rowsForHint [h] (len - 1)]
                    | otherwise = []
rowsForHint (h:hs) len = if len < 0 then []
                         else [(replicate h Filled) ++ [Blank] ++ rs | rs <- rowsForHint hs (len - h - 1)] ++
                              [Blank : rs | rs <- rowsForHint (h:hs) (len - 1)]

isValidSolution :: Solution -> Puzzle -> Bool
isValidSolution (Solution rs) (Puzzle rows cols) = areRowsValid cols (transpose rs)

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
