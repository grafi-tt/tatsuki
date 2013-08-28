{-# LANGUAGE BangPatterns, QuasiQuotes, TupleSections #-}
module Reversi.Tatsuki.Search where

import Prelude hiding (null)

import Control.Applicative hiding (empty)
import Control.Arrow

import Data.Bits
import Data.Int
import Data.IORef
import Data.PQueue.Max
import Reversi.Tatsuki.BitBoard
import Reversi.Tatsuki.Search.Parameter
import System.IO.Unsafe

import Language.Literals.Binary

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
minScore = -maxScore

evaluate :: Board -> IO Score
evaluate board@(pla, opp) =
  let !bCount = popCount pla
      !wCount = popCount opp
  in case (admissible &&& admissible . changeTurn) board of
    (0, 0) -> do
      -- TODO some logging here
      return $ case bCount `compare` wCount of
        GT -> maxScore - 1
        EQ -> 0
        LT -> minScore + 1

    (bAdm, wAdm) -> do
      -- TODO some logging here
      -- TODO use transposition table here
      -- TODO implement pattern-based evaluation

      let !allCount = bCount + wCount
          !diffCount = bCount - wCount

          !bAdmCount = popCount bAdm
          !wAdmCount = popCount wAdm
          !diffAdmCount = bAdmCount - wAdmCount

          !bCornerCount = countCorner pla
          !wCornerCount = countCorner opp
          !diffCornerCount = bCornerCount - wCornerCount

          !parity = (allCount .&. 1) `unsafeShiftL` 1 - 1

      return . fromIntegral $
        if allCount >= 56 then
          diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity `unsafeShiftL` 3 + diffCount
        else
          diffCornerCount `unsafeShiftL` 6 + diffAdmCount `unsafeShiftL` 3 + parity

-- Search
type Edge = Maybe BoardPos
foldEdge :: IteratePos t => (Edge -> a -> a) -> a -> t -> a
{-# INLINE foldEdge #-}
foldEdge f acc set | nullPos set = f Nothing acc
                   | otherwise   = foldPos (f . Just) acc set

moveBoard :: Edge -> Board -> Board
moveBoard Nothing board = board
moveBoard (Just pos) board = flipBoard pos board


orderEvaluate :: Depth -> Board -> IO Score
orderEvaluate depth board = (<$>) negate $ searchScore (shallowSearchDepth depth board) minScore maxScore $ changeTurn board

order :: Depth -> Board -> IO (MaxPQueue Score BoardPos)
order depth board = foldPos f (return empty) board
  where
    f pos pqM = do
      pq <- pqM
      score <- orderEvaluate depth $ flipBoard pos board
      return $ insert score pos pq


countCorner :: HemiBoard -> Int
countCorner = fromIntegral . popCount . (.&. [b|1000000100000000000000000000000000000000000000000000000010000001|])

findEdge :: Board -> IO Edge
findEdge board = searchEdge (searchDepth board) board

searchEdge :: Depth -> Board -> IO Edge
searchEdge !depth !board = snd <$> searchGeneral depth minScore maxScore board

searchScore :: Depth -> Score -> Score -> Board -> IO Score
searchScore !depth !α !β !board =
  case depth of
    0 -> evaluate board
    otherwise -> fst <$> searchGeneral depth α β board

-- calling searchGeneral with depth <= 0 causes infinite searching and stack overflow!!
-- TODO better type anotation, dealing with above problem
searchGeneral :: Depth -> Score -> Score -> Board -> IO (Score, Edge)
searchGeneral !depth !α !β !board
  | determineOrdering depth board = search' =<< order depth board
  | otherwise = search' board
  where
    search' :: IteratePos t => t -> IO (Score, Edge)
    search' = foldEdge f $ return (minScore, Nothing)
    f edge seM = do
      se@(score, _) <- seM
      if score >= β then
        return se
      else
        updateScore se <$> (( ,edge) . negate <$> searchScore (depth-1) (-β) (-(max α score)) (changeTurn $ moveBoard edge board))

updateScore :: (Score, Edge) -> (Score, Edge) -> (Score, Edge)
updateScore (score, edge) (score', edge') | score < score' = (score', edge')
                                          | otherwise      = (score, edge)
