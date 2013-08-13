{-# LANGUAGE BangPattern #-}

import Control.Applicative
import Control.Arrow

import Data.Array.Base (unsafeAt)
import Data.Bits

type HemiBoard = Word64
type Board = (HemiBoard, HemiBoard)
type HemiLine = Word8
type Line = (HemiLine, HemiLine)

type BoardPos = Word8
type LinePos = Word8

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

coord :: BoardPos -> (LinePos, LinePos)
coord = (%8) &&& (/8)

mkPosArray :: (BoardPos -> Int8) -> UArray LinePos Int8
mkPosArray f = array $ map (id *** f) [0..63]

uldrOffset :: BoardPos -> Int8
uldrOffset ((x,y) <- coord) = (x-y) * 8

uldrOffsetArray :: UArray BoardPos Int8
!uldrOffsetArray = mkPosArray uldrOffset

urdlOffset :: BoardPos -> Int8
urdlOffset ((x,y) <- coord) = (x+y-7) * 8

urdlOffsetArray :: UArray BoardPos Int8
!urdlOffsetArray = mkPosArray urdlOffset


-- TODO confirm the possibility of loop-unrolling by the compiler
admissible :: Board -> PosSet
admissible (!b, !w) =
  where
    !blank = not (b | w)
    !lrMask = [b|0111111001111110011111100111111001111110011111100111111001111110]
    !uldrMask = [b|0111111111111111111111111111111111111111111111111111111111111110]

    !lrw = lrMask & w

    !l1 = b `shiftL` 1 & lrw
    !l2 = l1 | l1 `shiftL` 1 & lrw
    !l3 = l2 | l2 `shiftL` 1 & lrw
    !l4 = l3 | l3 `shiftL` 1 & lrw
    !l5 = l4 | l4 `shiftL` 1 & lrw
    !l6 = l5 | l5 `shiftL` 1 & lrw
    !l7 = l6 `shift` 1

    !r1 = b `shiftR` 1 & lrw
    !r2 = r1 | r1 `shiftR` 1 & lrw
    !r3 = r2 | r2 `shiftR` 1 & lrw
    !r4 = r3 | r3 `shiftR` 1 & lrw
    !r5 = r4 | r4 `shiftR` 1 & lrw
    !r6 = r5 | r5 `shiftR` 1 & lrw
    !r7 = r6 `shiftR` 1


    !u1 = b `shiftL` 8
    !u2 = u1 | u1 `shiftL` 8 & w
    !u3 = u2 | u2 `shiftL` 8 & w
    !u4 = u3 | u3 `shiftL` 8 & w
    !u5 = u4 | u4 `shiftL` 8 & w
    !u6 = u5 | u5 `shiftL` 8 & w
    !u7 = u6 `shiftL` 8

    !d1 = b `shiftR` 8
    !d2 = d1 | d1 `shiftR` 8 & w
    !d3 = d2 | d2 `shiftR` 8 & w
    !d4 = d3 | d3 `shiftR` 8 & w
    !d5 = d4 | d4 `shiftR` 8 & w
    !d6 = d5 | d5 `shiftR` 8 & w
    !d7 = d6 `shiftR` 8


    !ul1 = b `shiftL` 9
    !ul2 = ul1 | ul1 `shiftL` 9 & w
    !ul3 = ul2 | ul2 `shiftL` 9 & w
    !ul4 = ul3 | ul3 `shiftL` 9 & w
    !ul5 = ul4 | ul4 `shiftL` 9 & w
    !ul6 = ul5 | ul5 `shiftL` 9 & w
    !ul7 = ul6 `shiftL` 9

    !dr1 = b `shiftR` 9
    !dr2 = dr1 | dr1 `shiftR` 9 & w
    !dr3 = dr2 | dr2 `shiftR` 9 & w
    !dr4 = dr3 | dr3 `shiftR` 9 & w
    !dr5 = dr4 | dr4 `shiftR` 9 & w
    !dr6 = dr5 | dr5 `shiftR` 9 & w
    !dr7 = dr6 `shiftR` 9


    !urdlw = urdlMask & w

    !ur1 = b `shiftL` 7
    !ur2 = ur1 | ur1 `shiftL` 7 & urdlw
    !ur3 = ur2 | ur2 `shiftL` 7 & urdlw
    !ur4 = ur3 | ur3 `shiftL` 7 & urdlw
    !ur5 = ur4 | ur4 `shiftL` 7 & urdlw
    !ur6 = ur5 | ur5 `shiftL` 7 & urdlw
    !ur7 = ur6 `shiftL` 7

    !dl1 = b `shiftR` 7
    !dl2 = dl1 | dl1 `shiftR` 7 & urdlw
    !dl3 = dl2 | dl2 `shiftR` 7 & urdlw
    !dl4 = dl3 | dl3 `shiftR` 7 & urdlw
    !dl5 = dl4 | dl4 `shiftR` 7 & urdlw
    !dl6 = dl5 | dl5 `shiftR` 7 & urdlw
    !dl7 = dl6 `shiftR` 7

    -- TODO The compiler may know associativity and do optimization.
    (((l7 | r7) | (u7 | d7)) | ((ul7 | ur7) | (dl7 | dr7))) & blank

flipLine :: LinePos -> Line -> HemiLine
flipLine !pos (!b, !w) = inner `xor` mask
  where
    !selector = pos `shiftL` 8

    !inner = unsafeAt lineArray (selector | not w)
    !outer = unsafeAt lineArray (selector | b)
    !mask = unsafeAt linkArray (selector | (inner `xor` outer))


flipBoard :: Board -> Board
flipBoard pos (b, w) = 
  where
    !x = pos & 7
    !x8 = x `shiftL` 3
    !y = pos `shiftR` 3
    !y8 = y `shiftL` 3

    !lrLine = (b `shiftR` y8) & 255

    !vertMul = [b|0000000100000010000001000000100000010000001000000100000010000000|]
    !udMask = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    !ud = (b `shiftR` x) & udMask
    !udLine = (ud * vertMul) `shiftR` 56

    !diagMul = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    !uldrMask = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    !uldr = (b `rotateL` (x8 - y8)) & uldrMask
    !(uldrMask `shiftR` pos & lineMask) * b & uldrMask

    !urdl = b `rotateR` (x8 + y8 - 56)
    !urdlMask = [b|0000000100000010000001000000100000010000001000000100000010000000|]
    !(urdlMask `shiftR` pos & lineMask) * b & urdlMask

