{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (map, pack, splitOn, strip, unpack)


type Range = (Int, Int)
type Pair = (Range, Range)


contains :: Range -> Range -> Bool
contains (a, b) (c, d) = a <= c && b >= d || a >= c && b <= d


overlaps :: Range -> Range -> Bool
overlaps (a, b) (c, d) = a <= d && b >= c


part1 :: [Pair] -> String
part1 = format . show . length . filter (uncurry contains)
    where format answer = "One range fully contains the other in " ++ answer ++ " assignment pairs."


part2 :: [Pair] -> String
part2 = format . show . length . filter (uncurry overlaps)
    where format answer = "The ranges overlap in " ++ answer ++ " pairs."


deserialize :: Text -> [Pair]
deserialize =
    let repl '-' = ','
        repl c   = c
        deserializeLine = combine . map (read . T.unpack) . T.splitOn ","
        combine (a:b:c:d:es) = ((a, b), (c, d))
    in map deserializeLine . T.splitOn "\n" . T.map repl . T.strip


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)

main = do
    pairs <- deserialize <$> TIO.getContents
    present [part1 pairs, part2 pairs]
