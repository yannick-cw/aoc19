{-# LANGUAGE LambdaCase #-}

module Two where

import           Data.List.Index                ( setAt )
import           Safe                           ( atMay )

data Opcode = Add Int Int Int | Mult Int Int Int | Stop deriving (Eq)

compute :: [Int] -> Either String [Int]
compute input = computeOpcode input 0

parseOpcode :: [Int] -> Either String Opcode
parseOpcode [1, pos1, pos2, outPos] = Right (Add pos1 pos2 outPos)
parseOpcode [2, pos1, pos2, outPos] = Right (Mult pos1 pos2 outPos)
parseOpcode (99 : _)                = Right Stop
parseOpcode invalid = Left $ "Got Invalid Opcode: " ++ show invalid

computeOpcode :: [Int] -> Int -> Either String [Int]
computeOpcode intcodes pos =
  parseOpcode (take 4 (drop pos intcodes))
    >>= (\case
          Stop                    -> Right intcodes
          (Add  pos1 pos2 outPos) -> ccc pos1 pos2 outPos (+)
          (Mult pos1 pos2 outPos) -> ccc pos1 pos2 outPos (*)
        )
 where
  ccc pos1 pos2 outPos op = do
    res <- liftMay $ op <$> atMay intcodes pos1 <*> atMay intcodes pos2
    computeOpcode (setAt outPos res intcodes) (pos + 4)



liftMay :: Maybe a -> Either String a
liftMay = maybe (Left "Error, did not find Element in intcodes") Right
