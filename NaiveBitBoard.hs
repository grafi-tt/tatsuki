module NaiveBitBoard where

import Data.Bits
import Data.Word

admissible :: (Word64, Word64) -> Word64
admissible brd@(pla, opp) = foldr f 0 [0..63]
  where
    f pos acc | (pla .|. opp) .&. 1 `shiftL` pos /= 0 = acc
              | flipped pos brd == 0 = acc
              | otherwise = acc .|. 1 `shiftL` pos

flipped :: Int -> (Word64, Word64) -> Word64
flipped pos (pla, opp) = foldr (.|.) 0 $ map flipped' dirs
  where
    dirs = [(x, y) | x <- [-1,0,1], y <- [-1,0,1], not (x == 0 && y == 0)]
    flipped' (dx, dy) =
      let loop acc (x, y) | overflow p' = 0
                          | check pla p' = acc
                          | check opp p' = loop (acc .|. place p') p'
                          | otherwise = 0
            where p' = (x+dx, y+dy)
      in loop 0 (posx, posy)

    posx = pos .&. 7
    posy = pos `shiftR` 3

    check set p = set .&. place p /= 0
    place (x, y) =  1 `shiftL` (x .|. y `shiftL` 3)
    overflow (x, y) = x == -1 || x == 8 || y == -1 || y == 8
