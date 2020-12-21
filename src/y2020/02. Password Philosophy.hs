{- AoC Day 2 - Password Philosphy - in Haskell.
 -
 - Testing some Either values.
 -
 - Author: Lucas Moeskops
 - Date: 2020-12-21
 -}

import Control.Applicative ( liftA2 )
import Data.Either ( either )
import Data.Function ( on )
import Text.ParserCombinators.Parsec
    ( alphaNum, char, digit, letter, string, many1, sepBy, parse, ParseError, 
      CharParser )

type Policy = (Int, Int, Char, String)

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

isValid :: Policy -> Bool
isValid (l, h, c, s) = liftA2 (&&) (l<=) (h>=) (count (==c) s)

isValidNew :: Policy -> Bool
isValidNew (l, h, c, s) = on (/=) ((==c).(s!!).subtract 1) l h

parsePolicy :: CharParser () Policy
parsePolicy = do low <- read <$> many1 digit
                 char '-'
                 high <- read <$> many1 digit
                 char ' '
                 letter <- letter
                 string ": "
                 password <- many1 alphaNum
                 return (low, high, letter, password)

parsePolicies :: String -> Either ParseError [Policy]
parsePolicies = parse (sepBy parsePolicy (char '\n')) "unable to parse"

main :: IO ()
main = do policies <- parsePolicies <$> getContents
          let part1 = count isValid <$> policies
          let part2 = count isValidNew <$> policies
          putStr $  "1: " ++ either show show part1
                 ++ "\n2: " ++ either show show part2
                 ++ "\n"