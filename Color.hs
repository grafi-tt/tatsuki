module Reversi.Color where

-- IOUArrayを使う都合上、石の色を表すのにIntを使う
type Color = Int
-- 0: None
-- 1: White
-- 2: Black
-- 3: Sentinel（番兵）

none  = 0 :: Color
white = 1 :: Color
black = 2 :: Color
sentinel   = 3 :: Color

oppositeColor :: Color -> Color
oppositeColor i = (2 - i) + 1

showColor c | c == white = "WHITE"
showColor c | c == black = "BLACK"
