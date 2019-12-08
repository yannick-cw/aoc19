module InReader where

import           Text.ParserCombinators.Parsec

lineParser :: GenParser Char st [String]
lineParser = endBy line eol
 where
  line = many (noneOf ",\n")
  eol  = char '\n'

commaParser :: GenParser Char st [String]
commaParser = sepBy (many (noneOf ",")) (char ',')

parseDay1 :: String -> Either ParseError [String]
parseDay1 = parse lineParser "unknown"

parseDay2 :: String -> Either ParseError [String]
parseDay2 = parse commaParser "unknown"

