{-# LANGUAGE OverloadedStrings #-}

import Data.List (elemIndex, sort, stripPrefix)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text.IO as TIO (getContents)
import qualified Data.Text as T (append, pack, splitOn, strip, unpack)


type Name = String
type Size = Int
data DirectoryTree = DirectoryTree Name [DirectoryTree] [File]
data File = File Name Size


instance Show DirectoryTree where
    show (DirectoryTree name ds fs) =
        let items = map show ds ++ map show fs
        in name ++ "\n" ++ showItems items
        where
            showItem :: Text -> String -> String
            showItem prefix = T.unpack . foldl1 T.append . fit . T.splitOn "\n" . T.pack
                where fit = map (`T.append` "\n") . (\a -> head a:(map (T.append prefix) $ tail a))

            showItems :: [String] -> String
            showItems (a:b:items) = "├── " ++ showItem "|   " a ++ showItems (b:items)
            showItems (a:items)   = "└── " ++ showItem "    " a

instance Show File where
    show (File name size) = name ++ " (" ++ show size ++ ")"


-- FIXME: Quite inefficient as all subdirectories are computed again instead of stored in lookup table
part1 :: DirectoryTree -> String
part1 =
    let format answer = "The sum of the total sizes of those directories is " ++ answer ++ "."
    in format . show . sum . filter (<=100000) . map size . walk


part2 :: DirectoryTree -> String
part2 dt =
    let spaceToFree = size dt - 70000000 + 30000000
        format answer = "The total size of that directory is " ++ answer ++ "."
    in format . show . minimum . filter (>=spaceToFree) . map size . walk $ dt


readDiskStructure :: [String] -> DirectoryTree
readDiskStructure = readDiskStructure' [DirectoryTree "root" [] []]
    where
        startsWith :: String -> String -> Bool
        startsWith s = (==s) . take (length s)

        readDiskStructure' :: [DirectoryTree] -> [String] -> DirectoryTree
        readDiskStructure' (top:parent:root:stack) [] =
            readDiskStructure' (addDirectory parent (reverseOrder top) : root : stack) []
        readDiskStructure' stack [] = reverseOrder . last $ init stack
        readDiskStructure' (top:stack) (line:rest)
            | line == "$ cd .."                         =
                readDiskStructure' (addDirectory (head stack) (reverseOrder top) : tail stack) rest
            | startsWith "$ cd " line                   =
                readDiskStructure' (DirectoryTree (drop 5 line) [] [] : top:stack) rest
            | (\x -> '0' <= x && x <= '9') $ head line =
                let parseFile = (\(size, name) -> File (tail name) (read size)) . splitAt (fromJust $ elemIndex ' ' line)
                in readDiskStructure' (addFile top (parseFile line):stack) rest
            | otherwise                                 = readDiskStructure' (top:stack) rest

        reverseOrder :: DirectoryTree -> DirectoryTree
        reverseOrder (DirectoryTree name ds fs) = DirectoryTree name (reverse ds) (reverse fs)


addDirectory :: DirectoryTree -> DirectoryTree -> DirectoryTree
addDirectory (DirectoryTree name ds fs) d = DirectoryTree name (d:ds) fs


addFile :: DirectoryTree -> File -> DirectoryTree
addFile (DirectoryTree name ds fs) f = DirectoryTree name ds (f:fs)


walk :: DirectoryTree -> [DirectoryTree]
walk dt@(DirectoryTree name ds fs) = dt:(concat $ map walk ds)


size :: DirectoryTree -> Size
size (DirectoryTree name ds fs) = (sum $ map fileSize fs) + (sum $ map size ds)
    where fileSize (File name size) = size


present = do putStrLn . foldr1 (\a b -> a ++ '\n':b)


main = do
    structure <- readDiskStructure . map T.unpack . T.splitOn "\n" . T.strip <$> TIO.getContents
    present [show structure, part1 structure, part2 structure]
