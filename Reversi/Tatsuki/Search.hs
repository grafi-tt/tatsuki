{-# LANGUAGE BangPatterns, QuasiQuotes, TupleSections #-}
module Reversi.Tatsuki.Search where

import Prelude hiding (null)

import Control.Applicative hiding (empty)
import Control.Arrow

import Control.Monad.ST
import Data.Bits
import Data.Int
import Data.IORef
import Data.PQueue.Max
import Reversi.Tatsuki.BitBoard
import System.IO.Unsafe

import Language.Literals.Binary

-- Parameters
orderDepth = 3
searchDepth = 5

-- Global vars
data SearchLog = SearchLog
  { hoge :: Int
  }

emptyLog :: SearchLog
emptyLog = SearchLog { hoge = 0 }

searchLogRef :: IORef SearchLog
{-# NOINLINE searchLogRef #-}
searchLogRef = unsafePerformIO (newIORef emptyLog)

resetSearchLog :: IO ()
resetSearchLog = writeIORef searchLogRef emptyLog

getSearchLog :: IO SearchLog
getSearchLog = readIORef searchLogRef

-- TODO declare and use transpostion table typed as IOUArray similary as above

-- Evaluate
type Score = Int32

maxScore :: Score
maxScore = maxBound

minScore :: Score
minScore = (-maxBound)

evaluate :: Board -> IO Score
evaluate board@(black, white) =
  let !bCount = popCount black
      !wCount = popCount white
  in case (admissible &&& admissible . changeTurn) board of
    (0, 0) -> do
      -- TODO some logging here
      return $ case bCount `compare` wCount of
        GT -> maxBound
        EQ -> 0
        LT -> -maxBound

    (bAdm, wAdm) -> do
      -- TODO some logging here
      -- TODO use transposition table here
      -- TODO implement pattern-based evaluation

      let !allCount = bCount + wCount
          !diffCount = bCount - wCount

          !bAdmCount = popCount bAdm
          !wAdmCount = popCount wAdm
          !diffAdmCount = bAdmCount - wAdmCount

          !bCornerCount = countCorner black
          !wCornerCount = countCorner white
          !diffCornerCount = bCornerCount - wCornerCount

          !parity = (allCount .&. 1) `unsafeShiftL` 1 - 1

      return . fromIntegral $
        if allCount >= 56 then
          diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3+ parity `unsafeShiftL`3 + diffCount
        else
          diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity

-- Search
type Edge = Maybe BoardPos
foldEdge :: IteratePos t => (Edge -> a -> a) -> a -> t -> a
foldEdge f acc set | nullPos set = f Nothing acc
                   | otherwise   = foldPos (f . Just) acc set

moveBoard :: Edge -> Board -> Board
moveBoard Nothing board = board
moveBoard (Just pos) board = flipBoard pos board


orderEvaluate :: Board -> IO Score
orderEvaluate board = (<$>) negate $ searchScore orderDepth minScore maxScore $ changeTurn board

order :: Board -> IO (MaxPQueue Score BoardPos)
order board = foldPos f (return empty) board
  where
    f pos pqM = do
      pq <- pqM
      score <- orderEvaluate $ flipBoard pos board
      return $ insert score pos pq


countCorner :: HemiBoard -> Int
countCorner = fromIntegral . popCount . (.&. [b|1000000100000000000000000000000000000000000000000000000010000001|])

-- TODO adjust parameter
findEdge :: Board -> IO Edge
findEdge board = searchEdge depth board
  where
    depth = case popCount (admissible board) of
      count | count >= 50 -> 64 - count
            | otherwise   -> 8

searchEdge :: Int -> Board -> IO Edge
searchEdge !depth !board = snd <$> searchGeneral depth minScore maxScore board

searchScore :: Int -> Score -> Score -> Board -> IO Score
searchScore !depth !α !β !board = fst <$> searchGeneral depth α β board

searchGeneral :: Int -> Score -> Score -> Board -> IO (Score, Edge)
searchGeneral !depth !α !β !board
  | depth <= orderDepth = search' board
  | otherwise           = search' board
  where
    search' = foldEdge f $ return (minScore, Nothing)
    f edge seM = do
      (score, edge) <- seM
      if score >= β then
        seM
      else
        updateScore <$> seM <*>  (( ,edge) <$> searchScore (depth-1) (-β) (-(max α score)) (changeTurn $ moveBoard edge board))

updateScore :: (Score, Edge) -> (Score, Edge) -> (Score, Edge)
updateScore (score, edge) (score', edge') | score < score' = (score, edge)
                                          | otherwise      = (score', edge')
