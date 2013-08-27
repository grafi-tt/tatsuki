module Reversi.Client.Random (RandomClient) where

import Control.Applicative
import Control.Monad

import Data.Array.IO
import Data.Array.MArray
import Data.List ((\\))

import System.Random

import qualified Reversi.Client as C
import Reversi.Color
import Reversi.Command

-- Client declaration
newtype RandomClient = RandomClient Board
instance C.Client RandomClient where
  initialize = RandomClient <$> initBoard
  play (RandomClient board) = play board
  doMove (RandomClient board) mv color = RandomClient <$> (doMove board mv color)
  putInfo (RandomClient board) = putBoard board

-- 番兵付きの10x10配列
type Board = IOUArray (Int,Int) Int

initBoard :: IO Board
initBoard =
    do { board <- newArray ( (0,0), (9,9) ) none
       ; mapM_ (\i ->
                    do writeArray board (i,0) sentinel
                       writeArray board (i,9) sentinel
                       writeArray board (0,i) sentinel
                       writeArray board (9,i) sentinel) [0..9]
       ; writeArray board (4,4) white
       ; writeArray board (4,5) black
       ; writeArray board (5,4) black
       ; writeArray board (5,5) white
       ; return board }

isValidMove :: Board -> Color -> (Int,Int) -> IO Bool
isValidMove board color (i,j) =
    do { e <- readArray board (i,j)
       ; if e == none then
             isEffective board color (i,j)
         else
             return False }
-- 8方向
dirs = [ (i,j) | i <- [1,0,-1], j <- [1,0,-1] ] \\ [(0,0)]

-- 石を置いたときに、ひっくり返せるかどうか
isEffective :: Board -> Color -> (Int,Int) -> IO Bool
isEffective board color (i,j) =
    do ms <- flippableIndices board color (i,j)
       return $ not $ null $ ms

-- 石を置いたときにひっくり返えるところ
flippableIndices :: Board -> Color -> (Int,Int) -> IO [(Int,Int)]
flippableIndices board color (i,j) =
    do bs <- mapM (\(di,dj) -> flippableIndicesLine board color (di,dj) (i+di,j+dj)) dirs
       return $ concat bs

flippableIndicesLine board color (di,dj) (i,j) =
    checkLine (di,dj) (i,j) []
    where
      ocolor = oppositeColor color
      checkLine (di,dj) (i,j) r =
          do { c <- readArray board (i,j)
             ; if c == ocolor then
                   checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
               else
                   return [] }
      checkLine' (di,dj) (i,j) r =
          do { c <- readArray board (i,j)
             ; if c == ocolor then
                   checkLine' (di,dj) (i+di,j+dj) ( (i,j) : r )
               else if c == color then
                        return r
                    else
                        return [] }

{-
   boardをそのまま返すのは一般には危険。
   だが、今のdoMoveの使用方法ならば問題ない。
-}
doMove :: Board -> Mv -> Color -> IO Board
doMove board GiveUp  color = return board
doMove board Pass    color = return board
doMove board (M i j) color =
    do { ms <- flippableIndices board color (i,j)
       ; mapM_ (\(ii,jj) -> writeArray board (ii,jj) color) ms
       ; writeArray board (i,j) color
       ; return board }

-- 合法手
validMoves :: Board -> Color -> IO [ (Int,Int) ]
validMoves board color =
     filterM (isValidMove board color)
             [ (i,j) | i <- [1..8], j <- [1..8]]

-- とても弱い
-- 置けるところに置くだけのプログラム。
play :: Board -> Color -> IO Mv
play board color =
    do { ms <- validMoves board color
       ; case ms of
           [] -> return $ Pass
           _  ->
               do k <- getStdRandom $ randomR (0, length ms-1)
                  let (i,j) = ms !! k
                  return $ M i j }

-- 石の数
count :: Board -> Color -> IO Int
count board color =
    do { is <- filterM (\i ->
                            do e <- readArray board i
                               return $ if e == color then True else False)
                       [ (i,j) | i <- [1..8], j <- [1..8]]
       ; return $ length is }


-- 盤面の出力
putBoard :: Board -> IO ()
putBoard board =
    do putStrLn " |A B C D E F G H "
       putStrLn "-+----------------"
       mapM_ putBoardLine [1..8]
       putStrLn "  (X: Black,  O: White)"
    where
      putC c | c == none  = putStr " "
             | c == white = putStr "O"
             | c == black = putStr "X"
      putBoardLine j =
          do putStr $ show j ++ "|"
             mapM_ (\i -> do e <- readArray board (i,j)
                             putC e >> putStr " ") [1..8]
             putStrLn ""
