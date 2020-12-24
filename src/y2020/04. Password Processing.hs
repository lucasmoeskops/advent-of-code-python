{- AoC Day 4 - Password Processing - in Haskell.
 -
 - Parser juggling and experimenting.
 -
 - Author: Lucas Moeskops
 - Date: 2020-12-24
 -}

import Control.Applicative ( liftA2 )
import Data.Either ( either )
import Data.Maybe ( mapMaybe )
import Text.ParserCombinators.Parsec
    ( alphaNum, char, digit, noneOf, oneOf, satisfy, string, many1, notFollowedBy, sepBy,
      (<|>), parse, try, ParseError, CharParser )

data HeightUnit = Cm | Inch
type Color = String
data EyeColor = Amber | Blue | Brown | Grey | Green | Hazel | Other
type AnonymousProperty = (String, String)
data Property = BirthYear Int
              | IssueYear Int
              | ExpirationYear Int
              | Height Int HeightUnit
              | HairColor Color
              | EyeColor EyeColor
              | PassportID String
              | Property AnonymousProperty
type Password = [Property]

requiredProperties :: [String]
requiredProperties = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

validHex :: Char -> Bool
validHex c = 'a' <= c && c <= 'f' || '0' <= c && c <= '9'

parseHex :: CharParser () String
parseHex = do char '#'
              many1 (satisfy validHex)

parseMaybe :: CharParser () a -> CharParser () (Maybe a)
parseMaybe p = (Just <$> p) <|> Nothing <$ many1 (noneOf " \n")

maybeProperty :: CharParser () Property -> String -> Maybe Property
maybeProperty p s = case parse p "" s of { Right r -> Just r; Left e -> Nothing }

parseBirthYear :: CharParser () Property
parseBirthYear = BirthYear . read <$> many1 digit

parseIssueYear :: CharParser () Property
parseIssueYear = IssueYear . read <$> many1 digit

parseExpirationYear :: CharParser () Property
parseExpirationYear = ExpirationYear . read <$> many1 digit

parseHeight :: CharParser () Property
parseHeight = do value  <- read <$> many1 digit
                 unit   <- try (Cm <$ string "cm") <|> (Inch <$ string "in")
                 return $ Height value unit

parseHairColor :: CharParser () Property
parseHairColor = HairColor <$> parseHex

parseEyeColor :: CharParser () Property
parseEyeColor = do color <-  try (Amber <$ string "amb")
                         <|> try (Blue <$ string "blu")
                         <|> try (Brown <$ string "brn")
                         <|> try (Grey <$ string "gry")
                         <|> try (Green <$ string "grn")
                         <|> try (Hazel <$ string "hzl")
                         <|> Other <$ string "oth"
                   return $ EyeColor color

parsePassportId :: CharParser () Property
parsePassportId = PassportID <$> many1 digit

parseProperty :: CharParser () Property
parseProperty = do name <- many1 alphaNum
                   char ':'
                   value <- many1 (noneOf "\n ")
                   return . Property $ (name, value)

parsePropertyValue :: Property -> Maybe Property
parsePropertyValue (Property (k, v)) =
    let parser = case k of
                    "byr" -> Just parseBirthYear
                    "iyr" -> Just parseIssueYear
                    "eyr" -> Just parseExpirationYear
                    "hgt" -> Just parseHeight
                    "hcl" -> Just parseHairColor
                    "ecl" -> Just parseEyeColor
                    "pid" -> Just parsePassportId
                    _ -> Nothing
    in case parser of { Just p -> maybeProperty p v; Nothing -> Nothing }

parsePropertyValues :: Password -> Password
parsePropertyValues = mapMaybe parsePropertyValue

parsePassword :: CharParser () Password
parsePassword = let carefulSeparator = do oneOf "\n "
                                          notFollowedBy (char '\n')
                in sepBy parseProperty (try carefulSeparator)

parsePasswords :: String -> Either ParseError [Password]
parsePasswords = parse (sepBy parsePassword (string "\n\n")) "unable to parse"

getPropertyKey :: Property -> String
getPropertyKey (Property (k, _))  = k
getPropertyKey (BirthYear _)      = "byr"
getPropertyKey (IssueYear _)      = "iyr"
getPropertyKey (ExpirationYear _) = "eyr"
getPropertyKey (Height _ _)       = "hgt"
getPropertyKey (HairColor _)      = "hcl"
getPropertyKey (EyeColor _)       = "ecl"
getPropertyKey (PassportID _)     = "pid"

hasRequiredProperties :: Password -> Bool
hasRequiredProperties ps = let keys = map getPropertyKey ps
                           in all (`elem` keys) requiredProperties

validateProperty :: Property -> Bool
validateProperty (BirthYear y)      = liftA2 (&&) (1920 <=) (<= 2002) y
validateProperty (IssueYear y)      = liftA2 (&&) (2010 <=) (<= 2020) y
validateProperty (ExpirationYear y) = liftA2 (&&) (2020 <=) (<= 2030) y
validateProperty (Height n Cm)      = liftA2 (&&) (150 <=) (<= 193) n
validateProperty (Height n Inch)    = liftA2 (&&) (59 <=) (<= 76) n
validateProperty (HairColor c)      = length c == 6
validateProperty (PassportID x)     = length x == 9
validateProperty _                  = True

validatePassword :: Password -> Bool
validatePassword = liftA2 (&&) hasRequiredProperties (all validateProperty)

main :: IO ()
main = do passwords <- parsePasswords <$> getContents
          let part1 = length . filter hasRequiredProperties <$> passwords
          let part2 = length . filter validatePassword . map parsePropertyValues <$> passwords
          putStr $  "1: " ++ either show show part1
                 ++ "\n2: " ++ either show show part2 ++ "\n"