module Lib
  ( someFunc
  )
where

import           One
import           Two
import           InReader
import           System.IO                      ( withFile
                                                , IOMode(ReadMode)
                                                , hGetContents
                                                )

someFunc :: IO ()
someFunc = sequence_ [runDay1 requiredFuel, runDay2 compute, runDay2 findInput]

runDay1 :: Show a => ([Int] -> a) -> IO ()
runDay1 fn = withFile "data/day1.txt" ReadMode $ \handle -> do
  file <- hGetContents handle
  case parseDay1 file of
    Right ll  -> print $ fn $ read <$> ll
    Left  err -> fail $ show err

runDay2 :: Show a => ([Int] -> a) -> IO ()
runDay2 fn = withFile "data/day2.txt" ReadMode $ \handle -> do
  file <- hGetContents handle
  case parseDay2 file of
    Right ll  -> print $ fn $ read <$> ll
    Left  err -> fail $ show err
