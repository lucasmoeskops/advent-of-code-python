{- AoC Day 1 - Report Repair - in Haskell.
 -
 - Using a set to lookup missing numbers.
 -
 - Author: Lucas Moeskops
 - Date: 2020-12-19
 -}

{-# LANGUAGE TupleSections #-}

import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe, fromMaybe, listToMaybe)
import qualified Data.Set as Set

findMatch :: Int -> Set.Set Int -> Maybe Int
findMatch t s = listToMaybe . Set.toList $ Set.filter (\n -> (t - n) `Set.member` s) s

main :: IO ()
main = do numbers <- map read . splitOn "\n" <$> getContents
          let s = Set.fromList numbers
          let part1 = let n = fromMaybe 0 (findMatch 2020 s) in n * (2020 - n)
          let part2 = let findTriplet n = (n,) <$> findMatch (2020 - n) s
                          calc (n, m) = n * m * (2020 - n - m)
                      in (calc.head.mapMaybe findTriplet) numbers
          putStr $ "1: " ++ show part1 ++ "\n2: " ++ show part2 ++ "\n"
