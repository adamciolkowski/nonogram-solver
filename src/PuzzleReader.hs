module PuzzleReader where
import System.IO

import Puzzle

readPuzzle filepath = do handle <- openFile filepath ReadMode
                         cols <- hGetLine handle
                         rows <- hGetLine handle
                         hClose handle
                         return (Puzzle (getHints cols) (getHints rows))
                         where
                            getHints hs = (read hs)::[[Int]]
