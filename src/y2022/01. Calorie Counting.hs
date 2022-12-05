import Data.List (sort)
import Data.Text (pack, splitOn, strip, unpack)

normalize = reverse . sort . map normalizeElf . splitOn (pack "\n\n") . strip . pack

normalizeElf = sum . map read . map unpack . splitOn (pack "\n")

main = do
    elfSums <- normalize <$> getContents
    putStrLn . show . (!!0) $ elfSums
    putStrLn . show . sum . take 3 $ elfSums
