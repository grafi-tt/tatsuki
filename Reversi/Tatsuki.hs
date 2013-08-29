module Reversi.Tatsuki
  ( SearchLog ()
  , Edge
  , resetSearchLog
  , getSearchLog
  , newNodeLog
  , modifyNodeLog
  , findEdge
  , moveBoard

  , HemiBoard, Board, BoardPos
  , initialBoard
  , flipBoard
  , changeTurn
  ) where

import Reversi.Tatsuki.BitBoard
import Reversi.Tatsuki.Search
