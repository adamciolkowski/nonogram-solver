import System.IO

readPuzzle path = do handle <- openFile path ReadMode
                     cols <- hGetLine handle
                     rows <- hGetLine handle
                     hClose handle
                     return (toList cols, toList rows)


toList s = (read s)::[[Int]]