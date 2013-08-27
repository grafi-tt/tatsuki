-- this file is based on the internally distributed sample implementation

{-# OPTIONS -XBangPatterns #-}

module Main where

import Reversi.Client
import Network
import System.Console.GetOpt
import System.Environment (getArgs)
import System.IO

import MainClient

data Config = Config { host :: HostName
                     , port :: PortNumber
                     , playerName :: String
                     , verbose :: Bool
                     , helpMode :: Bool
                     }

defaultConf = Config "localhost" 3000 "Anon." False False

options :: [OptDescr (Config -> Config)]
options =
  [ Option ['v'] ["verbose"]
           (NoArg $ \conf -> conf { verbose = True })
           "verbose mode"
  , Option ['H'] ["host"]
           (ReqArg (\s conf -> conf { host = s }) "HOST")
           "host name of a server"
  , Option ['p'] ["port"]
           (ReqArg (\s conf -> conf { port = fromIntegral (read s :: Int) }) "PORT")
           "port number of a server"
  , Option ['n'] ["name"]
           (ReqArg (\s conf -> conf { playerName = s }) "NAME")
           "player name"
  , Option ['h','?'] ["help"]
           (NoArg (\conf -> conf { helpMode = True }))
           "show this help"
  ]

usageMessage :: String
usageMessage = usageInfo header options
  where
    header = "Usage: \n" ++
             "    reversi -H HOST -p PORT -n NAME ...\n"

parseArg args =
  case getOpt Permute options args of
    (o, n, []) -> return (foldl (flip ($)) defaultConf o, n)
    (_, _, err) -> ioError (userError (concat err ++ usageMessage))

main = withSocketsDo $ do
  args <- getArgs
  (!conf, rest) <- parseArg args
  if helpMode conf then
    putStrLn usageMessage
  else
    runClient conf

runClient conf = do
  putStrLn $ "Connecting to " ++ (host conf) ++ " " ++ show (port conf)
  !h <- connectTo (host conf) (PortNumber $ port conf)
  putStrLn $ "Connection Ok."
  client <- (initialize :: IO ClientImp)
  doGame h client (playerName conf) (verbose conf)
