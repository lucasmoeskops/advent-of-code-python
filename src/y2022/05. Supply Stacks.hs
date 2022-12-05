import Data.List (transpose)
import Data.Text (Text, pack, splitOn, strip, unpack)


data Version = V9000 | V9001 deriving Eq

type Command = (Int, Int, Int)
type Crate = Char
type CrateSetup = [[Crate]]


useCrateMover :: Version -> CrateSetup -> [Command] -> String
useCrateMover version stacks [] = map head stacks
useCrateMover version stacks ((amount, from, to):ms) = useCrateMover version (move [] 1 stacks) ms
    where
        move :: CrateSetup -> Int -> CrateSetup -> CrateSetup
        move newStacks n [] = reverse newStacks
        move newStacks n oldStacks =
            let action | n == from = drop amount
                       | n == to   = ((adjust . take amount . (!!(from - 1)) $ stacks)++)
                       | otherwise = id
            in move ((action . head $ oldStacks):newStacks) (n+1) (drop 1 oldStacks)

        adjust :: [a] -> [a]
        adjust = if version == V9000 then reverse else id


parse :: String -> (CrateSetup, [Command])
parse contents =
    let [stacks, movements] = splitOn (pack "\n\n") . pack $ contents
    in (parseStacks stacks, parseCommands movements)


parseStacks :: Text -> CrateSetup
parseStacks =
      map (init . dropWhile (==' '))
    . every 4
    . (["", ""]++)
    . transpose
    . map unpack
    . splitOn (pack "\n")


parseCommands :: Text -> [Command]
parseCommands = map parseCommand . splitOn (pack "\n") . strip


parseCommand :: Text -> Command
parseCommand s =
    let parts = map unpack . splitOn (pack " ") $ s
    in (read $ parts!!1, read $ parts!!3, read $ parts!!5)


every :: Int -> [a] -> [a]
every n xs =
    case drop (n-1) xs of
        y : ys -> y : every n ys
        []     -> []


main = do
    (stacks, movements) <- parse <$> getContents
    putStrLn . show $ useCrateMover V9000 stacks movements
    putStrLn . show $ useCrateMover V9001 stacks movements
