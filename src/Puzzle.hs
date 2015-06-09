module Puzzle where

data Puzzle = Puzzle [[Int]] [[Int]]

data Cell = Filled | Blank deriving Eq

instance Show Cell where
    show Filled = "x"
    show Blank = "_"

data Solution = Solution [[Cell]]
