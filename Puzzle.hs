module Puzzle where

data Puzzle = Puzzle [[Int]] [[Int]]

data Cell = Filled | Blank deriving Eq

data Solution = Solution [[Cell]]
