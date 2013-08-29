{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Reversi.Tatsuki.Search.Parameter where

import Data.Bits
import Data.Int
import Reversi.Tatsuki.BitBoard

import Language.Literals.Binary

type Depth = Int
type Score = Int32

getSearchDepth :: Board -> Depth
{-# INLINE getSearchDepth #-}
getSearchDepth (pla, opp) =
  case popCount pla + popCount opp of
    count | count >= 48 -> 64 - count
          | count >= 44 -> 11
          | count >= 40 -> 10
          | count >= 12 -> 9
          | count >= 8  -> 10
          | otherwise   -> 11

getOrderDepth :: Depth -> Board -> Depth
{-# INLINE getOrderDepth #-}
getOrderDepth depth _ | depth > 7 = 3
                      | depth > 5 = 2
                      | otherwise = 0

getOrderEval :: Depth -> Board -> (Board -> IO Score)
{-# INLINE getOrderEval #-}
getOrderEval depth (pla, opp) =
  case popCount pla + popCount opp of
    count | count >= 48 -> finOrderEvaluate
          | otherwise -> evaluate

evaluate :: Board -> IO Score
{-# INLINE evaluate #-}
evaluate board@(pla, opp) =
  let !bCount = popCount pla
      !wCount = popCount opp
      !bAdm = admissible board
      !wAdm = admissible $ changeTurn board
      !allCount = bCount + wCount
      !diffCount = bCount - wCount

      !bAdmCount = popCount bAdm
      !wAdmCount = popCount wAdm
      !diffAdmCount = bAdmCount - wAdmCount

      !bCornerCount = countCorner pla
      !wCornerCount = countCorner opp
      !diffCornerCount = bCornerCount - wCornerCount

      !parity = (allCount .&. 1) `unsafeShiftL` 1 - 1
  in return . fromIntegral $
      if allCount >= 56 then
        diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity `unsafeShiftL` 3 + diffCount
      else
        diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity

countCorner :: HemiBoard -> Int
countCorner = fromIntegral . popCount . (.&. [b|1000000100000000000000000000000000000000000000000000000010000001|])

finOrderEvaluate :: Board -> IO Score
finOrderEvaluate board = return $ negate $ fromIntegral $ popCount $ admissible $ changeTurn board
