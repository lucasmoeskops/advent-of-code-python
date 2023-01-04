{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (pack, splitOn, strip, unpack)


type Round = (Int, Int)


part1 :: [Round] -> String
part1 =
    let format answer = "The total score if everything goes exactly according to my strategy guide would be " ++ answer ++ "."
    in format . show . sum . map roundScore
    where
        playScore = (+1)
        matchScore (opponent, me) = (me - opponent + 1) `mod` 3 * 3
        roundScore round@(opponent, me) = playScore me + matchScore round


part2 :: [Round] -> String
part2 =
    let format answer = "After following the Elf's instructions, the total score if everything goes exactly according to my strategy guide would be " ++ answer ++ "."
    in format . show . sum . map roundScore
    where
        resultScore = (*3)
        playChoice (opponent, me) = (opponent + me + 2) `mod` 3 + 1
        roundScore round@(opponent, me) = resultScore me + playChoice round


deserialize :: Text -> [Round]
deserialize = map (deserializeRound . T.unpack) . T.splitOn "\n" . T.strip


deserializeRound :: String -> Round
deserializeRound round =
    let indexAt i = fromJust . elemIndex (round!!i)
    in (indexAt 0 "ABC", indexAt 2 "XYZ")


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)


main = do
    rounds <- deserialize <$> TIO.getContents
    present [part1 rounds, part2 rounds]
