{-# LANGUAGE ViewPatterns, BangPatterns, QuasiQuotes, TypeSynonymInstances, FlexibleInstances #-}
module Reversi.Tatsuki.BitBoard where

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

-- TODO wrap HemiBoard by newtype
-- TODO defining data constructor having strict fields for Board and Line in conjuction with -funbox-strict-fields may produce better code, while the improvement probably be limited since unpacking Board does not occupy large part of logic.
type HemiBoard = Word64
type Board = (HemiBoard, HemiBoard)
type BoardPos = Int

type Line = Word8

class IteratePos t where
  foldPosTill :: (BoardPos -> a -> a) -> (BoardPos -> Bool) -> a -> t -> a
  nullPos :: t -> Bool -- It is a bit dirty, but needed for efficient handling of pass, which I cannot handle efficiently only with fodling.

-- not used because of hand inlining
-- TODO used these func instead of explicit recursion
{-
instance IteratePos HemiBoard where
  foldPosTill _ _ !acc 0 = acc
  foldPosTill f g !acc !tmp
    | nullPos tmp || g acc = acc
    | otherwise = let !lso = tmp .&. (-tmp)
                      !tmp' = tmp - lso
                      !pos = fromIntegral $ popCount (lso - 1)
                  in  foldPosTill f g (f pos acc) tmp'
  nullPos 0 = True
  nullPos _ = False

instance IteratePos Board where
  foldPosTill f g acc brd = foldPosTill f g acc $ admissible brd
  nullPos = nullPos . admissible

instance Ord k => IteratePos (MaxPQueue k BoardPos) where
  foldPosTill f g acc pq
    | nullPos pq || g acc = acc
    | otherwise = let ((_, pos), pq') = deleteFindMax pq
                  in  foldPosTill f (f pos acc) pq'
  nullPos = null
-}


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

outFlankArray :: UArray Int Line
outFlankArray = listArray (0, 511) $ fromIntegral . f <$> [0 .. 511]
  where
    split ix = (ix .&. 7, (fromIntegral $ ix ↪ 3) ↩ 1)
    f :: Int -> BoardPos
    f (split -> (pos, line)) = loop (↩ 1) (init ↩ 1) False .|. loop (↪ 1) (init ↪ 1) False
      where
        init = 1 ↩ pos
        loop f place ok
          | (place .&. 255 == 0) = 0
          | (place .&. line) /= 0 = loop f (f place) True
          | ok = place
          | otherwise = 0

flippedLineArray :: UArray Int Line
flippedLineArray = listArray (0, 1095) $ fromIntegral . f <$> [0 .. 1095]
  where
    split ix = (ix .&. 7, fromIntegral $ ix ↪ 3)
    f :: Int -> BoardPos
    f (split -> (pos, line)) = loop (↩ 1) (init ↩ 1) 0 .|. loop (↪ 1) (init ↪ 1) 0
      where
        init = 1 ↩ pos
        loop f place acc
          | (place .&. 255 == 0) = 0
          | (place .&. line) == 0 = loop f (f place) (acc .|. place)
          | otherwise = acc

flipLine :: Int -> HemiBoard -> HemiBoard -> HemiBoard
{-# INLINE flipLine #-}
flipLine !intPos !plaLine !oppLine = fromIntegral flippedLine
  where
    !outFlank    = unsafeAt outFlankArray    $ fromIntegral oppLine ↩ 2 .&. 504 .|. intPos
    !flippedLine = unsafeAt flippedLineArray $ fromIntegral (fromIntegral outFlank .&. plaLine) ↩ 3 .|. intPos


-- TODO confirm the possibility of loop-unrolling by the compiler
-- TODO move common subexpression into function, which will be inlined by the compiler and generate same code
initialBoard :: Board
initialBoard =
  ( [b|0000000000000000000000000000100000010000000000000000000000000000|]
  , [b|0000000000000000000000000001000000001000000000000000000000000000|]
  )

admissible :: Board -> HemiBoard
admissible (!pla, !opp) = (((l7 .|. r7) .|. (u7 .|. d7)) .|. ((ul7 .|. ur7) .|. (dl7 .|. dr7))) .&. blank
  where
    blank = complement (pla .|. opp)
    lrMask = [b|0111111001111110011111100111111001111110011111100111111001111110|]

    opp' = lrMask .&. opp

    l1 = pla ↩ 1 .&. opp'
    l2 = l1 .|. l1 ↩ 1 .&. opp'
    l3 = l2 .|. l2 ↩ 1 .&. opp'
    l4 = l3 .|. l3 ↩ 1 .&. opp'
    l5 = l4 .|. l4 ↩ 1 .&. opp'
    l6 = l5 .|. l5 ↩ 1 .&. opp'
    l7 = l6 ↩ 1

    r1 = pla ↪ 1 .&. opp'
    r2 = r1 .|. r1 ↪ 1 .&. opp'
    r3 = r2 .|. r2 ↪ 1 .&. opp'
    r4 = r3 .|. r3 ↪ 1 .&. opp'
    r5 = r4 .|. r4 ↪ 1 .&. opp'
    r6 = r5 .|. r5 ↪ 1 .&. opp'
    r7 = r6 ↪ 1


    u1 = pla ↩ 8 .&. opp
    u2 = u1 .|. u1 ↩ 8 .&. opp
    u3 = u2 .|. u2 ↩ 8 .&. opp
    u4 = u3 .|. u3 ↩ 8 .&. opp
    u5 = u4 .|. u4 ↩ 8 .&. opp
    u6 = u5 .|. u5 ↩ 8 .&. opp
    u7 = u6 ↩ 8

    d1 = pla ↪ 8 .&. opp
    d2 = d1 .|. d1 ↪ 8 .&. opp
    d3 = d2 .|. d2 ↪ 8 .&. opp
    d4 = d3 .|. d3 ↪ 8 .&. opp
    d5 = d4 .|. d4 ↪ 8 .&. opp
    d6 = d5 .|. d5 ↪ 8 .&. opp
    d7 = d6 ↪ 8


    ul1 = pla ↩ 9 .&. opp'
    ul2 = ul1 .|. ul1 ↩ 9 .&. opp'
    ul3 = ul2 .|. ul2 ↩ 9 .&. opp'
    ul4 = ul3 .|. ul3 ↩ 9 .&. opp'
    ul5 = ul4 .|. ul4 ↩ 9 .&. opp'
    ul6 = ul5 .|. ul5 ↩ 9 .&. opp'
    ul7 = ul6 ↩ 9

    dr1 = pla ↪ 9 .&. opp'
    dr2 = dr1 .|. dr1 ↪ 9 .&. opp'
    dr3 = dr2 .|. dr2 ↪ 9 .&. opp'
    dr4 = dr3 .|. dr3 ↪ 9 .&. opp'
    dr5 = dr4 .|. dr4 ↪ 9 .&. opp'
    dr6 = dr5 .|. dr5 ↪ 9 .&. opp'
    dr7 = dr6 ↪ 9


    ur1 = pla ↩ 7 .&. opp'
    ur2 = ur1 .|. ur1 ↩ 7 .&. opp'
    ur3 = ur2 .|. ur2 ↩ 7 .&. opp'
    ur4 = ur3 .|. ur3 ↩ 7 .&. opp'
    ur5 = ur4 .|. ur4 ↩ 7 .&. opp'
    ur6 = ur5 .|. ur5 ↩ 7 .&. opp'
    ur7 = ur6 ↩ 7

    dl1 = pla ↪ 7 .&. opp'
    dl2 = dl1 .|. dl1 ↪ 7 .&. opp'
    dl3 = dl2 .|. dl2 ↪ 7 .&. opp'
    dl4 = dl3 .|. dl3 ↪ 7 .&. opp'
    dl5 = dl4 .|. dl4 ↪ 7 .&. opp'
    dl6 = dl5 .|. dl5 ↪ 7 .&. opp'
    dl7 = dl6 ↪ 7


flipBoard :: BoardPos -> Board -> Board
flipBoard !pos (!pla, !opp) = ((pla `xor` fl) .|. 1 ↩ fromIntegral pos, opp `xor` fl)
  where fl = flipped pos (pla, opp)

flipped :: BoardPos -> Board -> HemiBoard
flipped !pos (!pla, !opp) = (lrFlip .|. udFlip) .|. (xyuldrFlip .|. xyurdlFlip)
--flipped !pos (!pla, !opp) = ((lrFlip .|. udFlip) .|. (xyuldrFlip .|. xyurdlFlip)) .&. 0 .|. xyurdlFlip
  where
    x = pos .&. 7
    y = pos ↪ 3

    xyuldr = (y - x) .&. 7
    xyurdl = (x + y - 7) .&. 7

    vertMul = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    diagMul = [b|0000000100000001000000010000000100000001000000010000000100000001|]

    udMask = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    uldrMask = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    urdlMask = [b|0000000100000010000001000000100000010000001000000100000010000000|]

    diagBorderMask = [b|0111111001111110011111100111111001111110011111100111111001111110|]

    toLRLine brd = brd ↪ (y ↩ 3) .&. 255
    toUDLine brd = (((brd ↪ x) .&. udMask) * vertMul) ↪ 56
    toULDRLine brd = (((brd ↺ (xyuldr ↩ 3)) .&. uldrMask) * diagMul) ↪ 56
    toURDLLine brd = (((brd ↺ (xyurdl ↩ 3)) .&. urdlMask) * diagMul) ↪ 56

    fromLRLine line = fromIntegral line ↩ (y ↩ 3)
    fromUDLine line = ((line * vertMul) .&. (udMask ↩ 7)) ↪ (7 - x)
    fromULDRLine line = ((line * diagMul) .&. uldrMask) ↻ (xyuldr ↩ 3)
    fromURDLLine line = ((line * diagMul) .&. urdlMask) ↻ (xyurdl ↩ 3)

    lrFlip = fromLRLine $ flipLine x (toLRLine pla) (toLRLine opp)
    udFlip = fromUDLine $ flipLine (7-y) (toUDLine pla) (toUDLine opp)
    xyuldrFlip = fromULDRLine $ flipLine x (toULDRLine pla) (toULDRLine opp .&. diagBorderMask ↺ xyuldr)
    xyurdlFlip = fromURDLLine $ flipLine x (toURDLLine pla) (toURDLLine opp .&. diagBorderMask ↻ xyurdl)

changeTurn :: Board -> Board
changeTurn (bk, wt) = (wt, bk)
