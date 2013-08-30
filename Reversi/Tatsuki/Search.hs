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


order :: EvalSel -> Depth -> Board -> IO (MaxPQueue Score BoardPos)
{-# INLINE order #-}
order eval orderDepth board = loop empty $ admissible board
  where
    loop pq !adm
      | adm == 0 = return pq
      | otherwise = do
        let lso = adm .&. (-adm)
            adm' = adm - lso
            !pos = fromIntegral $ popCount (lso - 1)
        score <- (<$>) negate $ searchScore eval (orderDepth-1) minScore (-(findMaxScore pq)) $ changeTurn $ flipBoard pos board
        loop (insert score pos pq) adm'
    findMaxScore pq | null pq = minScore
                    | otherwise = fst $ findMax pq

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
  | otherwise = Just . snd <$> searchEdge' depth minScore maxScore board

searchEdge' :: Depth -> Score -> Score -> Board -> IO (Score, BoardPos)
{-# INLINE searchEdge' #-}
searchEdge' !depth !α !β !board =
  searchOrdered (minScore, 0) =<< order (getOrderEval depth board) (getOrderDepth depth board) board
  where
    searchOrdered sp@(score, _) pq =
      let ((_, !pos), pq') = deleteFindMax pq
      in do
          sp' <- ( ,pos) . negate <$> searchScore tagEvaluate ((384 `xor` depth) - 1) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
          let sp''@(score'', _) = updateScore sp sp'
          if score'' >= β || null pq' then
            return sp''
          else
            searchOrdered sp'' pq'

updateScore :: (Score, BoardPos) -> (Score, BoardPos) -> (Score, BoardPos)
updateScore (score, pos) (score', pos') | score < score' = (score', pos')
                                        | otherwise      = (score, pos)

{-
    searchAdmissible sp@(score, _) !adm
      | score >= β || adm == 0 = return sp
      | otherwise =
        let lso = adm .&. (-adm)
            adm' = adm - lso
            !pos = fromIntegral $ popCount (lso - 1)
        in do
            sp' <- ( ,pos) . negate <$> searchScore eval (127 .&. (depth-1)) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
            searchAdmissible (updateScore sp sp') adm'
 -}

{-
    searchNegaScout sp@(score, _) pq = do
      let ((_, !pos), pq') = deleteFindMax pq
      score' <- negate <$> searchScore eval (127 .&. (depth-1)) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
      if score' >= β then
        return (score', pos)
      else
        let loop spl@(scorel, _) pql
              | null pql = return spl
              | otherwise = do
                let ((_, !posl), pql') = deleteFindMax pql
                scorel' <- negate <$> searchScore eval (127 .&. (depth-1)) (-(max α scorel + 1)) (-(max α scorel)) (changeTurn $ flipBoard pos board)
                if scorel' >= β then
                  return (scorel', posl)
                else
                  let spl' = updateScore spl (scorel', posl)
                  in if (max α scorel) >= scorel' then
                       loop spl' pql'
                     else
                       searchNegaScout spl' pql
        in loop (score', pos) pq'
 -}


searchScore :: EvalSel -> Depth -> Score -> Score -> Board -> IO Score
searchScore eval !depth !α !β !board@(pla, opp) =
  case (popCount pla, popCount opp) of
    (plaCnt, oppCnt)
      | plaCnt + oppCnt == 63 ->
        case flipBoard (popCount (complement (pla .|. opp) - 1)) board of
          (pla', opp') ->
            return $ case popCount pla' `compare` popCount opp' of
              GT -> maxScore - 1
              EQ -> 0
              LT -> minScore + 1
      | otherwise ->
        case (admissible board, admissible $ changeTurn board) of
          (plaAdm, oppAdm)
            | plaAdm == 0 && oppAdm == 0 ->
              return $ case plaCnt `compare` oppCnt of
                GT -> maxScore - 1
                EQ -> 0
                LT -> minScore + 1
            | depth <= 0 -> getActualEval eval board
            | plaAdm == 0 && depth == 1 -> negate <$> getActualEval eval (changeTurn board)
            | plaAdm == 0 -> negate <$> searchScore' eval (127 .&. (depth-1)) (-β) (-α) (changeTurn board)
            | otherwise -> searchScore' eval depth α β board

searchScore' :: EvalSel -> Depth -> Score -> Score -> Board -> IO Score
{-# INLINE searchScore' #-}
searchScore' eval !depth !α !β !board =
  case getOrderDepth depth board of
    orderDepth
      | orderDepth > 0 -> searchOrdered minScore =<< order (getOrderEval depth board) orderDepth board
      | otherwise -> searchAdmissible minScore $ admissible board
  where
    searchAdmissible score !adm =
      let lso = adm .&. (-adm)
          adm' = adm - lso
          !pos = fromIntegral $ popCount (lso - 1)
      in do
          score' <- negate <$> searchScore eval (127 .&. (depth-1)) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
          let score'' = max score score'
          if score'' >= β || adm' == 0 then
            return score''
          else
            searchAdmissible score'' adm'

    searchOrdered score pq =
      let ((_, !pos), pq') = deleteFindMax pq
      in do
          score' <- negate <$> searchScore eval (127 .&. (depth-1)) (-β) (-(max α score)) (changeTurn $ flipBoard pos board)
          let score'' = max score score'
          if score'' >= β || null pq' then
            return score''
          else
            searchOrdered score'' pq'
