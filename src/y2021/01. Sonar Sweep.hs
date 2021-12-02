{- AoC Day 1 - Sonar Sweep - in Haskell.
 -
 - Author: Lucas Moeskops
 - Date: 2021-12-01
 -}

slidingWindow size ns = if null$drop (size - 1) ns then [] else (sum$take size ns) : slidingWindow size (drop 1 ns)

task_1 numbers = length$filter (==True)$zipWith (<) numbers (drop 1 numbers)
task_2 numbers = length$filter (==True)$zipWith (<) window (drop 1 window) where window = slidingWindow 3 numbers

{- Using a + b + c < b + c + d === a < d -}
task_2_cp numbers = length$filter (==True)$zipWith (<) numbers (drop 3 numbers)

parse :: String -> [Int]
parse = map read.lines

main = do
    numbers <- parse <$> getContents
    putStrLn$(++) "[Part 1] [Comparing pairs]: ".show$task_1 numbers
    putStrLn$(++) "[Part 2] [Sliding window]: ".show$task_2 numbers
    putStrLn$(++) "[Part 3] [Comparing pairs]: ".show$task_2_cp numbers
