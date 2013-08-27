{-# LANGUAGE ViewPatterns, BangPatterns, QuasiQuotes, TypeSynonymInstances, FlexibleInstances #-}
module Reversi.BitBoard (
  HemiBoard, Board, BoardPos,
  IteratePos,
  foldPos,
  nullPos,
  admissible,
  flipBoard,
  changeTurn
) where

import Prelude hiding (null)

import Control.Applicative
import Control.Arrow

import Data.Array.Base (unsafeAt)
import Data.Array.Unboxed
import Data.Bits
import Data.Int
import Data.Word
import Data.PQueue.Max

import Language.Literals.Binary

-- TODO defining data constructor having strict fields for Board and Line in conjuction with -funbox-strict-fields may produce better code, while the improvement probably be limited since unpacking Board does not occupy large part of logic.
type HemiBoard = Word64
type Board = (HemiBoard, HemiBoard)
type HemiLine = Word8
type Line = (HemiLine, HemiLine)
type BoardPos = Word8
type LinePos = Word8

class IteratePos t where
  foldPos :: (BoardPos -> a -> a) -> a -> t -> a
  nullPos :: t -> Bool -- It is a bit dirty, but needed for efficient handling of pass, which I cannot handle efficiently only with fodling.

instance IteratePos HemiBoard where
  -- if f is strict for it's argument, no boxing needed (while it's probably notorious premature optimization, and I don't even complehend behavior in conjunction with foldEdge...)
  {-# INLINE foldPos #-}
  foldPos _ acc 0 = acc
  foldPos f !acc !tmp =
    let !lso = tmp .&. (-tmp)
        !tmp' = tmp - lso
        !pos = fromIntegral $ popCount (lso - 1)
    in  foldPos f (f pos acc) tmp'
  nullPos 0 = False
  nullPos _ = True

instance IteratePos Board where
  foldPos f acc = foldPos f acc . admissible
  nullPos = nullPos . admissible

instance Ord k => IteratePos (MaxPQueue k BoardPos) where
  foldPos f acc pq | null pq   = acc
                   | otherwise = let ((_, pos), pq') = deleteFindMax pq
                                 in  foldPos f (f pos acc) pq'
  nullPos = null


infixl 8 ↩, ↪, ↻, ↺

(↩) :: Bits a => a -> Int -> a
(↩) = unsafeShiftL
(↪) :: Bits a => a -> Int -> a
(↪) = unsafeShiftR
(↻) :: Bits a => a -> Int -> a
(↻) = rotateL
(↺) :: Bits a => a -> Int -> a
(↺) = rotateR


{-
bit layout:
LSB first
3F 3E 3D 3C 3B 3A 39 38
37 36 35 34 33 32 31 30
2F 2E 2D 2C 2B 2A 29 28
27 26 25 24 23 22 21 20
1F 1E 1D 1C 1B 1A 19 18
17 16 15 14 13 12 11 10
0F 0E 0D 0C 0B 0A 09 08
07 06 05 04 03 02 01 00
-}


outFlankArray :: UArray Int HemiLine
outFlankArray = listArray (0, 511) $ f <$> [0 .. 511]
  where
    split ix = (ix .&. 7, (fromIntegral $ ix ↪ 3) ↩ 1)
    f (split -> (pos, line)) = loop (↩ 1) (init ↩ 1) False .|. loop (↪ 1) (init ↪ 1) False
      where
        init = 1 ↩ pos
        loop f place ok
          | (place .&. 63 == 0) = 0
          | (place .&. line) /= 0 = loop f (f place) True
          | ok = place
          | otherwise = 0

flippedArray :: UArray Int HemiLine
flippedArray = listArray (0, 136) $ f <$> [0 .. 136]
  where
    split ix = (ix .&. 7, fromIntegral $ ix ↪ 3)
    f (split -> (pos, line)) = loop (↩ 1) (init ↩ 1) 0 .|. loop (↪ 1) (init ↪ 1) 0
      where
        init = 1 ↩ pos
        loop f place acc
          | (place .&. 63 == 0) = 0
          | (place .&. line) == 0 = loop f (f place) (acc .|. place)
          | otherwise = acc

flipLine :: Int -> HemiBoard -> HemiBoard -> HemiBoard
flipLine !intPos !blackLine !whiteLine = fromIntegral flipped
  where
    !outFlank = unsafeAt outFlankArray $ fromIntegral whiteLine ↩ 3 .|. intPos
    !flipped  = unsafeAt flippedArray  $ fromIntegral (fromIntegral outFlank .&. blackLine) ↩ 3 .|. intPos


-- TODO confirm the possibility of loop-unrolling by the compiler
admissible :: Board -> HemiBoard
admissible (!black, !white) = (((l7 .|. r7) .|. (u7 .|. d7)) .|. ((ul7 .|. ur7) .|. (dl7 .|. dr7))) .&. blank
  where
    blank = complement (black .|. white)
    lrMask = [b|0111111001111110011111100111111001111110011111100111111001111110|]

    lrw = lrMask .&. white

    l1 = black ↩ 1 .&. lrw
    l2 = l1 .|. l1 ↩ 1 .&. lrw
    l3 = l2 .|. l2 ↩ 1 .&. lrw
    l4 = l3 .|. l3 ↩ 1 .&. lrw
    l5 = l4 .|. l4 ↩ 1 .&. lrw
    l6 = l5 .|. l5 ↩ 1 .&. lrw
    l7 = l6 ↩ 1

    r1 = black ↪ 1 .&. lrw
    r2 = r1 .|. r1 ↪ 1 .&. lrw
    r3 = r2 .|. r2 ↪ 1 .&. lrw
    r4 = r3 .|. r3 ↪ 1 .&. lrw
    r5 = r4 .|. r4 ↪ 1 .&. lrw
    r6 = r5 .|. r5 ↪ 1 .&. lrw
    r7 = r6 ↪ 1


    u1 = black ↩ 8
    u2 = u1 .|. u1 ↩ 8 .&. white
    u3 = u2 .|. u2 ↩ 8 .&. white
    u4 = u3 .|. u3 ↩ 8 .&. white
    u5 = u4 .|. u4 ↩ 8 .&. white
    u6 = u5 .|. u5 ↩ 8 .&. white
    u7 = u6 ↩ 8

    d1 = black ↪ 8
    d2 = d1 .|. d1 ↪ 8 .&. white
    d3 = d2 .|. d2 ↪ 8 .&. white
    d4 = d3 .|. d3 ↪ 8 .&. white
    d5 = d4 .|. d4 ↪ 8 .&. white
    d6 = d5 .|. d5 ↪ 8 .&. white
    d7 = d6 ↪ 8


    ul1 = black ↩ 9
    ul2 = ul1 .|. ul1 ↩ 9 .&. lrw
    ul3 = ul2 .|. ul2 ↩ 9 .&. lrw
    ul4 = ul3 .|. ul3 ↩ 9 .&. lrw
    ul5 = ul4 .|. ul4 ↩ 9 .&. lrw
    ul6 = ul5 .|. ul5 ↩ 9 .&. lrw
    ul7 = ul6 ↩ 9

    dr1 = black ↪ 9
    dr2 = dr1 .|. dr1 ↪ 9 .&. lrw
    dr3 = dr2 .|. dr2 ↪ 9 .&. lrw
    dr4 = dr3 .|. dr3 ↪ 9 .&. lrw
    dr5 = dr4 .|. dr4 ↪ 9 .&. lrw
    dr6 = dr5 .|. dr5 ↪ 9 .&. lrw
    dr7 = dr6 ↪ 9


    ur1 = black ↩ 7
    ur2 = ur1 .|. ur1 ↩ 7 .&. lrw
    ur3 = ur2 .|. ur2 ↩ 7 .&. lrw
    ur4 = ur3 .|. ur3 ↩ 7 .&. lrw
    ur5 = ur4 .|. ur4 ↩ 7 .&. lrw
    ur6 = ur5 .|. ur5 ↩ 7 .&. lrw
    ur7 = ur6 ↩ 7

    dl1 = black ↪ 7
    dl2 = dl1 .|. dl1 ↪ 7 .&. lrw
    dl3 = dl2 .|. dl2 ↪ 7 .&. lrw
    dl4 = dl3 .|. dl3 ↪ 7 .&. lrw
    dl5 = dl4 .|. dl4 ↪ 7 .&. lrw
    dl6 = dl5 .|. dl5 ↪ 7 .&. lrw
    dl7 = dl6 ↪ 7



flipBoard :: BoardPos -> Board -> Board
flipBoard !pos (!black, !white) = ((black `xor` flip) .|. 1 ↩ fromIntegral pos, white `xor` flip)
  where
    x :: Int
    x = fromIntegral $ pos .&. 7
    y :: Int
    y = fromIntegral $ pos ↪ 3

    xyuldr = y - x
    xyurdl = x + y - 7

    vertMul = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    diagMul = [b|0000000100000001000000010000000100000001000000010000000100000001|]

    udMask = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    uldrMask = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    urdlMask = [b|0000000100000010000001000000100000010000001000000100000010000000|]

    toLRLine brd = brd ↪ (y ↩ 3) .&. 255
    toUDLine brd = (((brd ↪ x) .&. udMask) * vertMul) ↪ 56
    toULDRLine brd = (((brd ↺ (xyuldr ↩ 3)) .&. uldrMask) * diagMul) ↪ 56
    toURDLLine brd = (((brd ↺ (xyurdl ↩ 3)) .&. urdlMask) * diagMul) ↪ 56

    fromLRLine line = fromIntegral line ↩ (y ↩ 3)
    fromUDLine line = ((fromIntegral line * vertMul) .&. (udMask ↩ 7)) ↪ (7 - x)
    fromULDRLine line = ((fromIntegral line * diagMul) .&. uldrMask) ↩ (xyuldr ↩ 3)
    fromURDLLine line = ((fromIntegral line * diagMul) .&. urdlMask) ↩ (xyurdl ↩ 3)

    borderMask pos = r .&. l
      where l = 254 ↩ pos
            r = r ↪ 1

    lrFlip = fromLRLine $ flipLine x (toLRLine black) (toLRLine white)
    udFlip = fromUDLine $ flipLine y (toUDLine black) (toUDLine white)
    xyuldrFlip = fromULDRLine $ flipLine xyuldr (toULDRLine black) (toULDRLine white .&. borderMask xyuldr)
    xyurdlFlip = fromURDLLine $ flipLine xyurdl (toURDLLine black) (toURDLLine white .&. borderMask xyurdl)

    flip = (lrFlip .|. udFlip) .|. (xyuldrFlip .|. xyurdlFlip)

changeTurn :: Board -> Board
changeTurn (bk, wt) = (wt, bk)
