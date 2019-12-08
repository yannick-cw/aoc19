module InReader where

import           Text.ParserCombinators.Parsec
import           System.IO                      ( withFile
                                                , IOMode(ReadMode)
                                                , hGetContents
                                                )

lineParser :: GenParser Char st [String]
lineParser = endBy line eol
 where
  line = many (noneOf ",\n")
  eol  = char '\n'

parseIt :: String -> Either ParseError [String]
parseIt = parse lineParser "fuel_file"

run :: Show a => ([Int] -> a) -> IO ()
run fn = withFile "data/day1.txt" ReadMode $ \handle -> do
  file <- hGetContents handle
  case parseIt file of
    Right ll  -> print $ fn $ read <$> ll
    Left  err -> fail $ show err
