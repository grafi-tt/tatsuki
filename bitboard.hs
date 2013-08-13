{-# LANGUAGE BangPattern #-}

import Control.Applicative
import Control.Arrow

import Data.Array.Base (unsafeAt)
import Data.Bits

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

type HemiBoard = Word64
type Board = (HemiBoard, HemiBoard)
type HemiLine = Word8
type Line = (HemiLine, HemiLine)
type BoardPos = Word8
type LinePos = Word8

infixl 8 ↩ ↪ ↻ ↺
infixl 6 .⊻.

(↩) = unsafeShiftL
(↪) = unsafeShiftR
(↻) = rotateL
(↺) = rotateR
(.⊻.) = xor

-- http://hackage.haskell.org/package/binary-literal-qq
b = QuasiQuoter
  { quoteExp  = return . LitE . IntegerL . readBinary
  , quotePat  = return . LitP . IntegerL . readBinary
  , quoteType = error "No quasiquoter for types."
  , quoteDec  = error "No quasiquoter for declarations." }

readBinary :: String -> Integer
readBinary = foldl f 0 where
  f x '0' = shift x 1
  f x '1' = shift x 1 + 1
  f x ' ' = x
  f x _   = error "Not a valid binary literal."

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


mkPosArray :: (BoardPos -> Int8) -> UArray LinePos Int8
mkPosArray f = array $ map (id *** f) [0..63]



-- TODO confirm the possibility of loop-unrolling by the compiler
admissible :: Board -> HemiBoard
admissible (!b, !w) =
  where
    !blank = not (b .|. w)
    !lrMask = [b|0111111001111110011111100111111001111110011111100111111001111110]
    !uldrMask = [b|0111111111111111111111111111111111111111111111111111111111111110]

    !lrw = lrMask .&. w

    !l1 = b ↩ 1 .&. lrw
    !l2 = l1 | l1 ↩ 1 .&. lrw
    !l3 = l2 | l2 ↩ 1 .&. lrw
    !l4 = l3 | l3 ↩ 1 .&. lrw
    !l5 = l4 | l4 ↩ 1 .&. lrw
    !l6 = l5 | l5 ↩ 1 .&. lrw
    !l7 = l6 ↩ 1

    !r1 = b ↪ 1 .&. lrw
    !r2 = r1 | r1 ↪ 1 .&. lrw
    !r3 = r2 | r2 ↪ 1 .&. lrw
    !r4 = r3 | r3 ↪ 1 .&. lrw
    !r5 = r4 | r4 ↪ 1 .&. lrw
    !r6 = r5 | r5 ↪ 1 .&. lrw
    !r7 = r6 ↪ 1


    !u1 = b ↩ 8
    !u2 = u1 | u1 ↩ 8 .&. w
    !u3 = u2 | u2 ↩ 8 .&. w
    !u4 = u3 | u3 ↩ 8 .&. w
    !u5 = u4 | u4 ↩ 8 .&. w
    !u6 = u5 | u5 ↩ 8 .&. w
    !u7 = u6 ↩ 8

    !d1 = b ↪ 8
    !d2 = d1 | d1 ↪ 8 .&. w
    !d3 = d2 | d2 ↪ 8 .&. w
    !d4 = d3 | d3 ↪ 8 .&. w
    !d5 = d4 | d4 ↪ 8 .&. w
    !d6 = d5 | d5 ↪ 8 .&. w
    !d7 = d6 ↪ 8


    !ul1 = b ↩ 9
    !ul2 = ul1 | ul1 ↩ 9 .&. w
    !ul3 = ul2 | ul2 ↩ 9 .&. w
    !ul4 = ul3 | ul3 ↩ 9 .&. w
    !ul5 = ul4 | ul4 ↩ 9 .&. w
    !ul6 = ul5 | ul5 ↩ 9 .&. w
    !ul7 = ul6 ↩ 9

    !dr1 = b ↪ 9
    !dr2 = dr1 | dr1 ↪ 9 .&. w
    !dr3 = dr2 | dr2 ↪ 9 .&. w
    !dr4 = dr3 | dr3 ↪ 9 .&. w
    !dr5 = dr4 | dr4 ↪ 9 .&. w
    !dr6 = dr5 | dr5 ↪ 9 .&. w
    !dr7 = dr6 ↪ 9


    !urdlw = urdlMask .&. w

    !ur1 = b ↩ 7
    !ur2 = ur1 | ur1 ↩ 7 .&. urdlw
    !ur3 = ur2 | ur2 ↩ 7 .&. urdlw
    !ur4 = ur3 | ur3 ↩ 7 .&. urdlw
    !ur5 = ur4 | ur4 ↩ 7 .&. urdlw
    !ur6 = ur5 | ur5 ↩ 7 .&. urdlw
    !ur7 = ur6 ↩ 7

    !dl1 = b ↪ 7
    !dl2 = dl1 | dl1 ↪ 7 .&. urdlw
    !dl3 = dl2 | dl2 ↪ 7 .&. urdlw
    !dl4 = dl3 | dl3 ↪ 7 .&. urdlw
    !dl5 = dl4 | dl4 ↪ 7 .&. urdlw
    !dl6 = dl5 | dl5 ↪ 7 .&. urdlw
    !dl7 = dl6 ↪ 7

    -- TODO The compiler may know associativity and do optimization.
    (((l7 | r7) | (u7 | d7)) | ((ul7 | ur7) | (dl7 | dr7))) & blank

flipLine :: LinePos -> Line -> HemiLine
flipLine !pos (!b, !w) = inner .⊻. mask
  where
    !selector = pos ↩ 8

    !inner = unsafeAt lineArray (selector | not w)
    !outer = unsafeAt lineArray (selector | b)
    !mask = unsafeAt linkArray (selector | (inner .⊻. outer))

toLRLine :: BoardPos -> HemiBoard -> HemiLine
toLRLine pos brd = brd ↪ 

flipBoard :: BoardPos -> Board -> Board
flipBoard !pos (!b, !w) = 
  where
    pos' = fromIntegral pos :: Word64
    !x = pos' .&. 7
    !x8 = x ↩ 3
    !y = pos' ↪ 3
    !y8 = y ↩ 3
    !xyuldr = x - y
    !xyurdl = x + y - 7

    vertMul = [b|0000000100000010000001000000100000010000001000000100000010000000|]
    diagMul = [b|0000000100000001000000010000000100000001000000010000000100000001|]

    udMask = [b|0000000100000001000000010000000100000001000000010000000100000001|]
    uldrMask = [b|1000000001000000001000000001000000001000000001000000001000000001|]
    urdlMask = [b|0000000100000010000001000000100000010000001000000100000010000000|]

    toLRLine brd = brd ↪ y8 .&. 255
    toUDLine brd = (((brd ↪ x) .&. udMask) * vertMul) ↪ 56
    toULDRLine brd = (((brd ↻ (xyuldr ↩ 3)) .&. uldrMask) * diagMul) ↪ 56
    toURDLLine brd = (((brd ↺ (xyurdl ↩ 3)) .&. urdlMask) * diagMul) ↪ 56

    flipLine x (toLRLine b, toLRLine w)
    flipLine y (toUDLine b, toUDLine w)
    flipLine x (toULDRLine b, toULDRLine w )
    flipLine x (toURDLLine b, toURDLLine w)
