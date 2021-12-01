{- AoC Day 1 - Sonar Sweep - in Haskell.
 -
 - Author: Lucas Moeskops
 - Date: 2021-12-01
 -}

sliding_window size ns = if null$drop (size - 1) ns then [] else (sum$take size ns) : sliding_window size (drop 1 ns)

task_1 numbers = length$filter (==True)$zipWith (<) numbers (drop 1 numbers)
task_2 numbers = length$filter (==True)$zipWith (<) window (drop 1 window) where window = sliding_window 3 numbers

parse :: String -> [Int]
parse = map read.lines

main = do
    numbers <- parse <$> getContents
    putStrLn$show$task_1 numbers
    putStrLn$show$task_2 numbers
