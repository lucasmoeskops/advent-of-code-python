{-# LANGUAGE OverloadedStrings #-}

import Data.Array.Unboxed (UArray, (!), (//), listArray)
import Data.Char (ord)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (unpack)


findUniqueRange :: Int -> String -> Int
findUniqueRange size =
    let lastSeen = listArray (ord 'a', ord 'z') (repeat (-1)) :: UArray Int Int
    in  move lastSeen 0 0
    where
        move :: UArray Int Int -> Int -> Int -> String -> Int
        move state at uniqueSince (c:t)
            | at - uniqueSince == size = at
            | otherwise                =
                let state' = (state // [(ord c, at)])
                    uniqueSince' = (max (state ! ord c + 1) (uniqueSince))
                in move state' (at + 1) uniqueSince' t


part1 :: String -> String
part1 = format . show . findUniqueRange 4
    where format answer = answer ++ " Characters need to be processed before the first start-of-packet marker is detected."


part2 :: String -> String
part2 = format . show . findUniqueRange 14
    where format answer = answer ++ " Characters need to be processed before the first start-of-message marker is detected."


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)

main = do
    text <- T.unpack <$> TIO.getContents
    present [part1 text, part2 text]
