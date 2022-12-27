{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T (map, pack, splitOn, strip, unpack)

normalize :: String -> [Int]
normalize = reverse . sort . map normalizeElf . T.splitOn "\n\n" . T.strip . T.pack

normalizeElf :: Text -> Int
normalizeElf = sum . map (read . T.unpack) . T.splitOn "\n"

main = do
    elfSums <- normalize <$> getContents
    putStrLn . show . (!!0) $ elfSums
    putStrLn . show . sum . take 3 $ elfSums
