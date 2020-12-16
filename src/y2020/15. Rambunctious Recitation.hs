{- AoC Day 15 - Rambunctious Recitation - in Haskell.
 -
 - Runs in about 10 seconds at the moment. Not very fast.
 -
 - Author: Lucas Moeskops
 - Date: 2020-12-16
 -}

import Control.Monad (foldM)
import Data.Array.IO
import Data.List (intercalate)
import Data.List.Split (splitOn)

iteration :: IOArray Int Int -> Int -> Int -> IO Int
iteration a p i = 
    do x <- readArray a p
       writeArray a p i
       return $ if x > 0 then i - x else 0

find :: Int -> [Int] -> IO Int
find t initial =
    do arr <- newArray (0, t) 0 :: IO (IOArray Int Int)
       sequence_ [writeArray arr x i | (i, x) <- zip [1..] initial]
       foldM (iteration arr) (last initial) [length initial..t-1]

main :: IO ()
main = do
    x <- map read.splitOn "," <$> getLine
    out <- sequence [find i x | i <- [2020, 30000000]]
    print . intercalate ", " . map show $ out
