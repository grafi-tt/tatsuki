module Reversi.Tatsuki.Search.Parameter where

import Data.Bits
import Reversi.Tatsuki.BitBoard

type Depth = Int

searchDepth :: Board -> Depth
searchDepth board =
  case popCount (admissible board) of
    count | count >= 44 -> 64 - count
          | count >= 40 -> 16
          | count >= 36 -> 15
          | count >= 32 -> 12
          | count >= 28 -> 9
          | count >= 24 -> 8
          | count >= 12 -> 7
          | otherwise   -> 8

shallowSearchDepth :: Depth -> Board -> Depth
shallowSearchDepth _ _ = 3

determineOrdering :: Depth -> Board -> Bool
determineOrdering depth _ = depth > 5
