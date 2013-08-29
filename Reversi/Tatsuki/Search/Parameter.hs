{-# LANGUAGE BangPatterns, QuasiQuotes #-}

module Reversi.Tatsuki.Search.Parameter where

import Data.Bits
import Data.Int
import Reversi.Tatsuki.BitBoard

import Language.Literals.Binary

type Depth = Int
type EvalSel = Int
type Score = Int32

tagEvaluate :: EvalSel
tagEvaluate = 0
tagFinOrderEvaluate :: EvalSel
tagFinOrderEvaluate = 1

getActualEval :: EvalSel -> (Board -> IO Score)
{-# INLINE getActualEval #-}
getActualEval 0 = evaluate
getActualEval 1 = finOrderEvaluate

getSearchDepth :: Board -> Depth
{-# INLINE getSearchDepth #-}
getSearchDepth (pla, opp) =
  case popCount pla + popCount opp of
    count | count >= 48 -> 128 + 64 - count
          | otherwise   -> 128 + 10

getOrderDepth :: Depth -> Board -> Depth
{-# INLINE getOrderDepth #-}
getOrderDepth depth board@(pla, opp) =
  case popCount pla + popCount opp of
    count | (count + depth) .&. 64 == 64 ->
            case depth of
              _ | depth >= (128 + 15) -> 11
                | depth >= 13 -> 3
                | depth >= 4 -> 1
                | otherwise -> 0
          | otherwise -> (depth .&. 127) `unsafeShiftR` 1 - 1

getOrderEval :: Depth -> Board -> EvalSel
{-# INLINE getOrderEval #-}
{-
getOrderEval depth (pla, opp) =
  case (popCount pla + popCount opp) of
    count | depth >= (128 + 15) -> 0
          | (depth + count) .&. 64 == 64 -> 1
          | otherwise -> 0
-}
getOrderEval depth (pla, opp) =
  case (popCount pla + popCount opp) of
    count -> 1 .&. ((depth `unsafeShiftR` 7 .&. (depth + 1) `unsafeShiftR` 4) `xor` (depth + count) `unsafeShiftR` 6)

evaluate :: Board -> IO Score
{-# INLINE evaluate #-}
evaluate board@(pla, opp) =
  let !bCount = popCount pla
      !wCount = popCount opp
      !bAdm = admissible board
      !wAdm = admissible $ changeTurn board
      !allCount = bCount + wCount
      -- !diffCount = bCount - wCount

      !bAdmCount = popCount bAdm
      !wAdmCount = popCount wAdm
      !diffAdmCount = bAdmCount - wAdmCount

      !bCornerCount = countCorner pla
      !wCornerCount = countCorner opp
      !diffCornerCount = bCornerCount - wCornerCount

      !parity = (allCount .&. 1) `unsafeShiftL` 1 - 1
  in  return . fromIntegral $ diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity

countCorner :: HemiBoard -> Int
{-# INLINE countCorner #-}
countCorner = fromIntegral . popCount . (.&. [b|1000000100000000000000000000000000000000000000000000000010000001|])

finOrderEvaluate :: Board -> IO Score
{-# INLINE finOrderEvaluate #-}
finOrderEvaluate board@(pla, opp) =
  let !bCornerCount = countCorner pla
      !wCornerCount = countCorner opp
      !admCount = popCount $ admissible board
  in  return . fromIntegral $ admCount + (bCornerCount - wCornerCount)
