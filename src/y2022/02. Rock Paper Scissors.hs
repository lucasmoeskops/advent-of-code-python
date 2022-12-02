import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Text (pack, splitOn, unpack)

part1 = sum.map roundScore
    where
        playScore = (+1)
        matchScore (opponent, me) = (me - opponent + 1) `mod` 3 * 3
        roundScore round@(opponent, me) = playScore me + matchScore round

part2 = sum.map roundScore
    where
        resultScore = (*3)
        playChoice (opponent, me) = (opponent + me + 2) `mod` 3 + 1
        roundScore round@(opponent, me) = resultScore me + playChoice round

normalize = map normalizeRound . filter (/="") . map unpack . splitOn (pack "\n") . pack

normalizeRound round =
    let indexAt i = fromJust . elemIndex (round!!i)
    in (indexAt 0 "ABC", indexAt 2 "XYZ")

main = do
    rounds <- normalize <$> getContents
    putStrLn . show . part1 $ rounds
    putStrLn . show . part2 $ rounds
