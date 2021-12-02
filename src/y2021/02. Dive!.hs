{- AoC Day 2 - Dive! - in Haskell.
 -
 - Author: Lucas Moeskops
 - Date: 2021-12-02
 -}


task_1 = run 0 0
    where
        run position depth []                      = position * depth
        run position depth ((direction,amount):cs) =
            case direction of
                "forward" -> run (position + amount) depth            cs
                "up"      -> run position            (depth - amount) cs
                "down"    -> run position            (depth + amount) cs
task_2 = run 0 0 0
    where
        run position depth aim []                      = position * depth
        run position depth aim ((direction,amount):cs) =
            case direction of
                "forward" -> run (position + amount) (depth + amount * aim) aim            cs
                "up"      -> run position            depth                  (aim - amount) cs
                "down"    -> run position            depth                  (aim + amount) cs

parse :: String -> [(String, Int)]
parse = map parseLine.lines
    where parseLine = (\(direction:amount:ws) -> (direction, read amount)).words

main = do
    commands <- parse <$> getContents
    putStrLn$(++) "[Part 1]: ".show$task_1 commands
    putStrLn$(++) "[Part 2]: ".show$task_2 commands
