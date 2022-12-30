{-# LANGUAGE OverloadedStrings #-}

import Data.Char (ord)
import Data.List (elemIndex)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (pack, splitOn, strip, unpack)


part1 :: [String] -> String
part1 =
    let format answer = "The sum of the priorities of those item types is " ++ answer ++ "."
    in format . show . sum . map (priority . item_in_both_compartments)


part2 :: [String] -> String
part2 =
    let format answer = "The sum of the priorities of those item types is " ++ answer ++ "."
    in format . show . sum . findBadges
    where
        findBadges :: [String] -> [Int]
        findBadges [] = []
        findBadges (a:b:c:ds) = (priority $ badge a b c):findBadges ds


priority :: Char -> Int
priority letter
    | letter >= 'a' = ord(letter) - ord('a') + 1
    | otherwise     = ord(letter) - ord('A') + 27


item_in_both_compartments :: String -> Char
item_in_both_compartments xs =
    let (left, right) = splitAt ((`div` 2) $ length xs) xs
        search (y:ys) =
            case elemIndex y right of
                Just i -> y
                Nothing -> search ys
    in search left


badge :: String -> String -> String -> Char
badge a b c =
    let
        search (x:xs) =
            case elemIndex x b of
                Just i ->
                    case elemIndex x c of
                        Just j -> x
                        Nothing -> search xs
                Nothing -> search xs
    in search a


deserialize :: Text -> [String]
deserialize = map T.unpack . T.splitOn "\n" . T.strip


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)


main = do
    rucksacks <- deserialize <$> TIO.getContents
    present [part1 rucksacks, part2 rucksacks]
