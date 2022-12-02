import Data.List (sort)
import Data.Text (pack, splitOn, unpack)

normalize = reverse . sort . map normalizeElf . splitOn (pack "\n\n") . pack

normalizeElf = sum . map read . filter (/= "") . map unpack . splitOn (pack "\n")

main = do
    elfSums <- normalize <$> getContents
    putStrLn . show . (!!0) $ elfSums
    putStrLn . show . sum . take 3 $ elfSums
