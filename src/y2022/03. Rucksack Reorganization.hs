{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative (liftA2)
import Data.Char (ord)
import Data.List (elemIndex, intercalate, intersperse)
import qualified Data.Text as T (pack, splitOn, strip, unpack)


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


part1 :: [String] -> Int
part1 = sum . map (priority . item_in_both_compartments)


part2 :: [String] -> Int
part2 []         = 0
part2 (a:b:c:ds) = (priority $ badge a b c) + part2 ds


deserialize :: String -> [String]
deserialize = map T.unpack . T.splitOn "\n" . T.strip . T.pack


main = do
    rucksacks <- deserialize <$> getContents
    putStrLn . intercalate "\n" . map show . (`map` [part1, part2]) . flip ($) $ rucksacks
