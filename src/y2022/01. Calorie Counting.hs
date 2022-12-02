import Data.List (sort)
import Data.Text (pack, splitOn, unpack)

main = do
    numbers <- splitOn (pack "\n\n").pack <$> getContents
    let sums :: [Int] = map (sum.map read.filter (/= "").map unpack.splitOn (pack "\n")) numbers
    putStrLn $ show.maximum $ sums
    putStrLn $ show.sum.take 3.reverse.sort $ sums