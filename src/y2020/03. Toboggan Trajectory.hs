{- AoC Day 3 - Toboggan Trajectory - in Haskell.
 -
 - With data and types.
 -
 - Author: Lucas Moeskops
 - Date: 2020-12-21
 -}

import Data.Either ( either )
import Text.ParserCombinators.Parsec
    ( char, many1, sepBy, (<|>), parse, try, ParseError, CharParser )

data Square = Tree | Open deriving Show
type Level = [Square]
type Hill = [Level]
type Slope = (Int, Int)

slopes :: [Slope]
slopes = [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

parseSquare :: CharParser () Square
parseSquare = try (Tree <$ char '#') <|> (Open <$ char '.')

parseLevel :: CharParser () Level
parseLevel = many1 parseSquare

parseHill :: String -> Either ParseError Hill
parseHill = parse (sepBy parseLevel (char '\n')) "unable to parse"

countTrees :: Slope -> Hill -> Int
countTrees = countTrees' 0 0
    where countTrees' n x _          []        = n
          countTrees' n x s@(dx, dy) h@(y:ys)  =
              let increment = case cycle y !! x of { Open -> 0; Tree -> 1 }
              in countTrees' (n + increment) (x + dx) s (drop dy h)

main :: IO ()
main = do hill <- parseHill <$> getContents
          let part1 = countTrees (3, 1) <$> hill
          let part2 = product . (\h -> map (($h).countTrees) slopes) <$> hill
          putStr $  "1: " ++ either show show part1
                 ++ "\n2: " ++ either show show part2 ++ "\n"