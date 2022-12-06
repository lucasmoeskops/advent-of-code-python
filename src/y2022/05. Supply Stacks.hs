{-# LANGUAGE OverloadedStrings #-}

import Data.List (transpose)
import Data.Text.IO as IO (getContents)
import Data.Text (Text)
import qualified Data.Text as T


data Version = V9000 | V9001 deriving Eq

type Command = (Int, Int, Int)
type Crate = Char
type CrateSetup = [Text]


useCrateMover :: Version -> CrateSetup -> [Command] -> Text
useCrateMover version stacks [] = T.pack . map T.head $ stacks
useCrateMover version stacks ((amount, from, to):ms) = useCrateMover version (move [] (length stacks)) ms
    where
        move :: CrateSetup -> Int -> CrateSetup
        move newStacks 0 = newStacks
        move newStacks n =
            let action | n == from = T.drop amount
                       | n == to   = T.append . adjust . T.take amount . (!!(from - 1)) $ stacks
                       | otherwise = id
            in move ((action . (!!(n-1)) $ stacks):newStacks) (n-1)

        adjust :: Text -> Text
        adjust = if version == V9000 then T.reverse else id


parse :: Text -> (CrateSetup, [Command])
parse contents =
    let [stacks, movements] = T.splitOn "\n\n" $ contents
    in (parseStacks stacks, parseCommands movements)


parseStacks :: Text -> CrateSetup
parseStacks =
      map (T.init . T.dropWhile (==' '))
    . everyWithOffset 4 1
    . T.transpose
    . T.splitOn "\n"


parseCommands :: Text -> [Command]
parseCommands = map parseCommand . T.splitOn "\n" . T.strip


parseCommand :: T.Text -> Command
parseCommand s =
    let parts = map T.unpack . T.splitOn " " $ s
    in (read $ parts!!1, read $ parts!!3, read $ parts!!5)


everyWithOffset :: Int -> Int -> [a] -> [a]
everyWithOffset n o [] = []
everyWithOffset n 0 (x:xs) = x : everyWithOffset n (-1) xs
everyWithOffset n (-1) xs =
    case drop (n-1) xs of
        y : ys -> y : everyWithOffset n (-1) ys
        []     -> []
everyWithOffset n o xs =
    case drop o xs of
        y : ys -> y : everyWithOffset n (-1) ys
        []     -> []


main = do
    (stacks, movements) <- parse <$> IO.getContents
    putStrLn . show $ useCrateMover V9000 stacks movements
    putStrLn . show $ useCrateMover V9001 stacks movements
