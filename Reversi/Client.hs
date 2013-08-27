-- this file is based on the internally distributed sample implementation
module Reversi.Client where
  ( Client
  , doGame
  )

import Network

import Reversi.Color
import Reversi.Command
import Reversi.Play

class Client c where
  initialize :: IO c
  doMove :: c -> Mv -> Color -> IO c
  play :: c -> Color -> IO Mv
  putInfo :: c -> IO () -- not good

-- State Machine
doGame :: Client c => Handle -> c -> String -> Bool -> IO ()
doGame h client name verbose = do
  hPutCommand h $ Open name
  waitStart h verbose

waitStart :: Client c => Handle -> c -> Bool -> IO ()
waitStart h client verbose = do
  c <- hGetCommand' h
  case c of
   Bye scores -> putStrLn $ showScores scores
   Start color opname mytime -> do
     if color == black then
       performMyMove h client color [] opname mytime verbose
     else
       waitOpponentMove h client color [] opname mytime verbose
   _ -> error $ "Invalid Command: " ++ show c

performMyMove :: Client c => Handle -> c -> Color -> Hist -> String -> Int -> Bool -> IO ()
performMyMove h client color hist opname mytime verbose = do
  pmove <- play client color
  client <- doMove client pmove color
  hPutCommand h $ Move pmove
  when verbose $ putStrLn $ replicate 80 '-'
  when verbose $ putStrLn ("PMove: " ++ show pmove ++ " " ++ showColor color)
  when verbose (putBoard client)
  c <- hGetCommand' h
  case c of
    Ack mytime' ->
      waitOpponentMove h client color (PMove pmove:hist) opname mytime' verbose
    End wl n m r ->
      procEnd h client color hist opname wl n m r verbose
    _ ->
      error $ "Invalid Command: " ++ show c

waitOpponentMove :: Client c => Handle -> c -> Color -> Hist -> String -> Int -> Bool -> IO ()
waitOpponentMove h client color hist opname mytime verbose = do
  c <- hGetCommand' h
  case c of
    Move omove -> do
      client <- doMove client omove (oppositeColor color)
      when verbose $ putStrLn $ replicate 80 '-'
      when verbose $ putStrLn ("OMove: " ++ show omove ++ " " ++ showColor color)
      when verbose (putInfo client)
      performMyMove h client color (OMove omove:hist) opname mytime verbose
    End wl n m r ->
      procEnd h client color hist opname wl n m r verbose
    _ ->
      error $ "Invalid Command: " ++ show c

procEnd :: Client c => Handle -> c -> Color -> Hist -> String -> WL -> Int -> Int -> Config -> IO ()
procEnd h client color hist opname wl n m r conf = do
  case wl of
    Win ->
      putStrLn $ printf "You win! (%d vs. %d) -- %s." n m r
    Lose ->
      putStrLn $ printf "You lose! (%d vs. %d) -- %s." n m r
    Tie ->
      putStrLn $ printf "Draw (%d vs. %d) -- %s." n m r

    putStrLn $ printf "Your name: %s (%s)  Oppnent name: %s (%s)."
                      (playerName conf)
                      (showColor color)
                      opname (showColor (oppositeColor color))

    putInfo $ client
    putStrLn $ showHist hist
    waitStart h conf

-- History
data OPMove = OMove Mv | PMove Mv

instance Show OPMove where
  show (OMove mv) = "-" ++ show mv
  show (PMove mv) = "+" ++ show mv

type Hist = [OPMove]

showHist :: Hist -> String
showHist hist = foldr (\a r -> show a ++ " " ++ r) "" $ reverse hist

showScores :: [(String, Int)] -> String
showScores scores = foldr f "" scores
  where
    f (n,s) r = n ++ ":" ++ replicate (len + 1 - (length $ n)) ' ' ++ show s ++ "\n" ++ r
    len = maximum $ map (length . fst) scores

-- Network Helper
hPutCommand :: Handle -> Command -> IO ()
hPutCommand h c = do
  hPutStr h (show c)
  hPutStr h "\n"
  hFlush h
  putStrLn $ "Sent: " ++ show c

hGetCommand :: Handle -> IO (Command)
hGetCommand h = do
  r <- hGetLine h
  putStrLn $ "Received: " ++ r
  return $ either error id $ parseCommand r

hGetCommand' :: Handle -> IO (Command)
hGetCommand' h = do
  c <- hGetCommand h
  case c of
    Empty -> hGetCommand' h
    _     -> return c