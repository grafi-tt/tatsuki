{-# LANGUAGE BangPatterns, QuasiQuotes, TupleSections #-}
module Reversi.Tatsuki.Search where

import Prelude hiding (null)

import Control.Applicative hiding (empty)
import Control.Arrow

import Data.Bits
import Data.Int
import Data.IORef
import Data.PQueue.Max
import Data.Time.Clock
import Reversi.Tatsuki.BitBoard
import Reversi.Tatsuki.Search.Parameter
import System.IO.Unsafe

import Language.Literals.Binary

-- Global vars
type SearchLog = [NodeLog]

data NodeLog = NodeLog
  { timeLog :: NominalDiffTime
  , edgeLog :: Edge
  , turnLog :: Int
  } deriving Show

emptyLog :: SearchLog
emptyLog = []

initialNodeLog :: NodeLog
initialNodeLog = NodeLog
  { timeLog = 0
  , edgeLog = Nothing
  , turnLog = 0
  }

searchLogRef :: IORef SearchLog
{-# NOINLINE searchLogRef #-}
searchLogRef = unsafePerformIO (newIORef emptyLog)

resetSearchLog :: IO ()
resetSearchLog = writeIORef searchLogRef emptyLog

getSearchLog :: IO SearchLog
getSearchLog = readIORef searchLogRef

newNodeLog :: IO ()
newNodeLog = modifyIORef' searchLogRef (initialNodeLog :)

modifyNodeLog :: (NodeLog -> NodeLog) -> IO ()
modifyNodeLog f = modifyIORef' searchLogRef g
  where g (x:xs) = f x : xs
        g [] = error "`modifyNodeLog` is called for empty SearchLog"

-- TODO declare and use transpostion table typed as IOUArray similary as above

-- Evaluate
maxScore :: Score
maxScore = maxBound

minScore :: Score
minScore = -maxScore

-- Search
type Edge = Maybe BoardPos

moveBoard :: Edge -> Board -> Board
moveBoard Nothing board = board
moveBoard (Just pos) board = flipBoard pos board


order :: (Board -> IO Score) -> Depth -> Board -> IO (MaxPQueue Score BoardPos)
order eval orderDepth board = loop empty $ admissible board
  where
    loop pq !adm
      | adm == 0 = return pq
      | otherwise = do
        let lso = adm .&. (-adm)
            adm' = adm - lso
            !pos = fromIntegral $ popCount (lso - 1)
        score <- (<$>) negate $ searchScore eval orderDepth minScore maxScore $ changeTurn $ flipBoard pos board
        loop (insert score pos pq) adm'

findEdge :: Board -> IO Edge
findEdge board@(pla, opp) = do
  newNodeLog
  t1 <- getCurrentTime
  e <- searchEdge (getSearchDepth board) board
  t2 <- getCurrentTime
  let turn = popCount pla + popCount opp
  modifyNodeLog (\r -> r { timeLog = diffUTCTime t2 t1, edgeLog = e, turnLog = turn })
  return e

searchEdge :: Depth -> Board -> IO Edge
searchEdge !depth !board
  | admissible board == 0 = return Nothing
  | otherwise = Just . snd <$> searchGeneral evaluate depth minScore maxScore board

searchScore :: (Board -> IO Score) -> Depth -> Score -> Score -> Board -> IO Score
searchScore eval !depth !α !β !board@(pla, opp) =
  case (admissible board, admissible $ changeTurn board, depth) of
    (0, 0, _) ->
      return $ case popCount pla `compare` popCount opp of
        GT -> maxScore - 1
        EQ -> 0
        LT -> minScore + 1
    (_, _, 0) -> eval board
    (0, _, 1) -> negate <$> eval (changeTurn board)
    (0, _, _) -> negate <$> fst <$> searchGeneral eval (depth-1) (-β) (-α) (changeTurn board)
    _ -> fst <$> searchGeneral eval depth α β board

-- calling searchGeneral with depth <= 0 causes infinite searching and stack overflow!!
-- TODO better type anotation, dealing with above problem
searchGeneral :: (Board -> IO Score) -> Depth -> Score -> Score -> Board -> IO (Score, BoardPos)
searchGeneral eval !depth !α !β !board =
  case getOrderDepth depth board of
    0 -> searchAdmissible (minScore, 0) $ admissible board
    orderDepth -> searchOrdered (minScore, 0) =<< order (getOrderEval depth board) orderDepth board
  where
    searchAdmissible sp@(score, _) !adm
      | score >= β || adm == 0 = return sp
      | otherwise = do
        let lso = adm .&. (-adm)
            adm' = adm - lso
            !pos = fromIntegral $ popCount (lso - 1)
        sp' <- ( ,pos) . negate <$> searchScore eval (depth-1) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
        searchAdmissible (updateScore sp sp') adm'

    searchOrdered sp@(score, _) pq
      | score >= β || null pq = return sp
      | otherwise = do
        let ((_, !pos), pq') = deleteFindMax pq
        sp' <- ( ,pos) . negate <$> searchScore eval (depth-1) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
        searchOrdered (updateScore sp sp') pq'

updateScore :: (Score, BoardPos) -> (Score, BoardPos) -> (Score, BoardPos)
updateScore (score, pos) (score', pos') | score < score' = (score', pos')
                                        | otherwise      = (score, pos)
