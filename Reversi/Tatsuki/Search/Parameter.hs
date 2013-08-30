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
    count | count >= 46 -> 256 + 64 - count
          | otherwise   -> 256 + 10

getOrderDepth :: Depth -> Board -> Depth
{-# INLINE getOrderDepth #-}
getOrderDepth depth board@(pla, opp) =
  case popCount pla + popCount opp of
    count | (count + depth) .&. 64 == 64 ->
            case depth of
              _ | depth >= (256 + 17) -> 12
                | depth >= (256 + 14) -> 11
                | (255 .&. depth) >= (128 + 12) -> 7
                | depth >= 12 -> 3
                | depth >= 5 -> 1
                | otherwise -> 0
    otherwise -> (depth .&. 63) `unsafeShiftR` 1 - 1
    --  let upper = if depth .&. 63 >= 12 then depth `unsafeShiftR` 5 .&. 12 else 0
    --      base  = ((depth + 4) .&. 24) `unsafeShiftR` 2
    --  in  upper + base - 1


getOrderEval :: Depth -> Board -> EvalSel
{-# INLINE getOrderEval #-}
getOrderEval depth (pla, opp) =
  case (popCount pla + popCount opp) of
    count -> 1 .&. ((depth `unsafeShiftR` 8 .&. (depth + 4) `unsafeShiftR` 4) `xor` (depth + count) `unsafeShiftR` 6)

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
