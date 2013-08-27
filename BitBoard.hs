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


lineArray :: UArray Int HemiLine
lineArray = listArray (0, 2047) $ f <$> [0 .. 2047]
  where
    split ix = (ix ↪ 8, fromIntegral $ ix .&. 255)
    f (split -> (pos, line)) = (l7 .|. r7)
      where
        blank = complement line
        place = 1 ↩ pos

        l1 = place ↩ 1 .&. blank
        l2 = l1 .|. l1 ↩ 1 .&. blank
        l3 = l2 .|. l2 ↩ 1 .&. blank
        l4 = l3 .|. l3 ↩ 1 .&. blank
        l5 = l4 .|. l4 ↩ 1 .&. blank
        l6 = l5 .|. l5 ↩ 1 .&. blank
        l7 = if l6 .&. 128 == 0 then l6 else 0

        r1 = place ↪ 1 .&. blank
        r2 = r1 .|. r1 ↪ 1 .&. blank
        r3 = r2 .|. r2 ↪ 1 .&. blank
        r4 = r3 .|. r3 ↪ 1 .&. blank
        r5 = r4 .|. r4 ↪ 1 .&. blank
        r6 = r5 .|. r5 ↪ 1 .&. blank
        r7 = if r6 .&. 1 == 0 then r6 else 0


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


flipLine :: Int -> Line -> HemiLine
flipLine !intPos (!black, !white) = inner `xor` mask
  where
    selector = intPos ↩ 8

    !inner = unsafeAt lineArray $ selector .|. fromIntegral (complement white)
    !outer = unsafeAt lineArray $ selector .|. fromIntegral black
    !mask = unsafeAt lineArray $ selector .|. fromIntegral (inner `xor` outer)


flipBoard :: BoardPos -> Board -> Board
flipBoard !pos (!black, !white) = ((black `xor` flip) .|. 1 ↩ fromIntegral pos, white `xor` flip)
  where
    x :: Int
    x = fromIntegral $ pos .&. 7
    y :: Int
    y = fromIntegral $ pos ↪ 3

    xyuldr = x - y
    xyurdl = x + y - 7

    vertMul = [b|0000000100000010000001000000100000010000001000000100000010000000|]
    diagMul = [b|0000000100000001000000010000000100000001000000010000000100000001|]

    udMask = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    uldrMask = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    urdlMask = [b|0000000100000010000001000000100000010000001000000100000010000000|]

    toLRLine brd = fromIntegral $ brd ↪ (y ↩ 3) .&. 255
    toUDLine brd = fromIntegral $ (((brd ↪ x) .&. udMask) * vertMul) ↪ 56
    toULDRLine brd = fromIntegral $ (((brd ↻ (xyuldr ↩ 3)) .&. uldrMask) * diagMul) ↪ 56
    toURDLLine brd = fromIntegral $ (((brd ↺ (xyurdl ↩ 3)) .&. urdlMask) * diagMul) ↪ 56

    fromLRLine line = fromIntegral line ↩ (y ↩ 3)
    fromUDLine line = ((fromIntegral line .&. 127) * vertMul) ↪ (7 - x) .|. (fromIntegral line .&. 128) ↩ (49 + x)
    fromULDRLine line = (fromIntegral line * diagMul) .&. uldrMask
    fromURDLLine line = (fromIntegral line * diagMul) .&. urdlMask

    borderMask pos = r .&. l
      where l = 254 ↩ pos
            r = r ↪ 1

    lrFlip = fromLRLine $ flipLine x (toLRLine black, toLRLine white)
    udFlip = fromUDLine $ flipLine y (toUDLine black, toUDLine white)
    xyuldrFlip = fromULDRLine $ flipLine xyuldr (toULDRLine black, toULDRLine white .&. borderMask xyuldr)
    xyurdlFlip = fromURDLLine $ flipLine xyurdl (toURDLLine black, toURDLLine white .&. borderMask xyurdl)

    flip = (lrFlip .|. udFlip) .|. (xyuldrFlip .|. xyurdlFlip)

changeTurn :: Board -> Board
changeTurn (bk, wt) = (wt, bk)
