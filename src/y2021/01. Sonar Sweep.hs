{- AoC Day 1 - Sonar Sweep - in Haskell.
 -
 - Author: Lucas Moeskops
 - Date: 2021-12-01
 -}

slidingWindow size ns = if null$drop (size - 1) ns then [] else (sum$take size ns) : slidingWindow size (drop 1 ns)

task_1 ns = length$filter (==True)$zipWith (<) ns (drop 1 ns)
task_2 ns = length$filter (==True)$zipWith (<) window (drop 1 window) where window = slidingWindow 3 ns

{- Using a + b + c < b + c + d === a < d -}
task_2_cp ns = length$filter (==True)$zipWith (<) ns (drop 3 ns)

parse :: String -> [Int]
parse = map read.lines

main = do
    numbers <- parse <$> getContents
    putStrLn$(++) "[Part 1] [Comparing pairs]: ".show$task_1 numbers
    putStrLn$(++) "[Part 2] [Sliding window]: ".show$task_2 numbers
    putStrLn$(++) "[Part 3] [Comparing pairs]: ".show$task_2_cp numbers
