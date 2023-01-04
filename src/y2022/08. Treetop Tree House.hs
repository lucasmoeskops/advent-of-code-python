{-# LANGUAGE OverloadedStrings #-}

import Data.Array.Unboxed (UArray, (!), listArray)
import Data.Char (digitToInt)
import Data.Int (Int8)
import Data.List (elemIndex, nub, stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (findIndex, length, replace, strip, unpack)


type TreeMap = (Int, Int, UArray Int Int8)


part1 :: TreeMap -> String
part1 (width, height, lookup) =
    let lToR = [[i | i <- [j..(j + width - 1)]] | j <- map (*width) [0..(height - 1)]]
        rToL = [reverse row | row <- lToR]
        tToB = [[i | i <- map ((+j) . (*width)) [0..(height - 1)]] | j <- [0..(width - 1)]]
        bToT = [reverse col | col <- tToB]
        ranges = concat [lToR, rToL, tToB, bToT]
        format answer = answer ++ " trees are visible from outside the grid."
    in format . show . length . nub . concat . map sight $ ranges
    where
        sight :: [Int] -> [Int]
        sight (i:is) = sight' (lookup ! i) [i] is

        sight' :: Int8 -> [Int] -> [Int] -> [Int]
        sight' height match (i:is)
            | lookup ! i > height = sight' (lookup ! i) (i:match) is
            | otherwise           = sight' (max height (lookup ! i)) match is
        sight' _ match _ = match


part2 :: TreeMap -> String
part2 (width, height, lookup) =
    let format answer = "The highest scenic score possible for any tree is " ++ answer ++ "."
    in format . show . maximum . map scenicScore $ [0..(width * height - 1)]
    where
        scenicScore :: Int -> Int
        scenicScore = foldl1 (*) . map (\r -> length (sight r) - 1) . ranges

        ranges :: Int -> [[Int]]
        ranges i = [
                    [i,i-width..0],
                    [i,i+1..((i `div` width + 1)*width-1)],
                    [i,i+width..width*height-1],
                    [i,i-1..(i `div` width)*width]
                   ]

        sight :: [Int] -> [Int]
        sight (i:is) = sight' (lookup ! i) [i] is

        sight' :: Int8 -> [Int] -> [Int] -> [Int]
        sight' height match (i:is)
            | lookup ! i < height = sight' height (i:match) is
            | otherwise           = (i:match)
        sight' _ match _ = match


deserialize :: Text -> TreeMap
deserialize t =
    let t' = T.strip t
        len = T.length t'
        width = fromJust . T.findIndex (=='\n') $ t'
        height = (len + 1) `div` (width + 1)
    in (width, height, listArray (0, len - height) (values t'))
    where
        values = map (fromIntegral . digitToInt) . T.unpack . T.replace "\n" ""


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)


main = do
    treeMap <- deserialize <$> TIO.getContents
    present [part1 treeMap, part2 treeMap]
