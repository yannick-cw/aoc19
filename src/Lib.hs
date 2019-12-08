module Lib
  ( someFunc
  )
where

import           One
import           InReader

someFunc :: IO ()
someFunc = run requiredFuel
