{-# LANGUAGE NoMonomorphismRestriction, OverloadedStrings #-}

module ConnectionPool (
    Connection,
    ConnectionHandler,
    Pool,
    runConn,
    connFromPool
) where

--import Control.Applicative
--import Data.Attoparsec hiding (take, string, inClass)
--import Data.Attoparsec.Combinator
--import Data.Attoparsec.ByteString hiding (take, inClass)
--import Data.Attoparsec.ByteString.Char8 hiding (take, string)


import Network
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import System.IO
import Data.Map as M (empty, Map)

type Connection = (Handle, HostName, PortNumber)
type Pool id = M.Map id [(ThreadId, Connection)]
type ServerState id = (ConnectionRunner id, Socket, TVar (Pool id))
type ServerHandler id = ServerState id -> IO ()
type ConnectionState id = (TVar (Pool id), Connection)
type ConnectionHandler id = ConnectionState id -> IO ()
type ConsoleHandler = Handle -> Handle -> IO ()

data ConnectionRunner id = 
  ConnectionRunner {
    consoleHandler :: ConsoleHandler,
    idGenerator :: (Connection -> ConnectionState id -> ConnectionState id),
    connectionHandler :: ConnectionHandler id,
    goodByeHandler :: ConnectionHandler id,
    port :: Num}

main = runConn putChar nameToId echoHandler

nameToId :: Connection -> ConnectionState id -> ConnectionState id
nameToId c cs = do
  hdl <- fst' 
  hPutStrLn hdl "What's your name?"
  hGetLine hdl

fst' (a,b,c) = a
connFromPool = fst' . snd

-- runConn (ConnectionRunner consoleHdlr idGen connectionHdlr goodByeHdlr port) = 
runConn cr = withSocketsDo $ do
  s <- (listenOn (PortNumber $ port cr))
  p <- atomically (newTVar (M.empty::Pool String))
  t <- forkIO (repeatAccept cr s p)
  repeatUntilExit stdin stdout consoleHandler ""
  p' <- atomically $ readTVar p
  mapM_ killThread (t:map fst p')
  sClose s
  putStrLn "Enter to exit." >> getLine

repeatAccept (cr, s, p) = do
  c <- accept s
  t <- forkFinally (ch $ idGen c (p, c)) (exitPool p)
  atomically $ do
    p' <- readTVar p
    writeTVar p ((t,(c,"")):p')
  repeatAccept s p
  where
    ch = connectionHandler cr
    idGen = idGenerator cr
    

sayBye :: ConnectionHandler
sayBye s@(pool, conn) = do
  tid <- myThreadId
  print $ "Exiting: " ++ (show tid)
  p <- (atomically $ readTVar pool)
  h <- hdl p tid
  open <- hIsOpen h
  if open then do hPutStrLn h "bye\n" >> hFlush h >> hClose h else return ()
  where hdl p tid = return.connFromPool $ head $ filter ((==tid).fst) p
      
exitPool :: ConnectionState -> ConnectionHandler -> a -> IO ()
exitPool s@(pool, conn) handler = \_ -> do
  handler s
  atomically $ do
    pool' <- readTVar pool
    writeTVar pool $ filter ((/=tid).fst) pool'
    return ()

echoHandler :: ConnectionHandler id
echoHandler pool a@(hdl,_,_,_) = repeatUntilExit hdl hdl echoToHandleAndStdout ""
  where echoToHandleAndStdout x = hPutChar hdl x >> putChar x

repeatUntilExit :: Handle -> Handle -> (Char -> IO ()) -> [Char] -> IO ()
repeatUntilExit hIn hOut f "exit\n" = return ()
repeatUntilExit hIn hOut f x = do
  c <- hGetChar hIn
  f c
  repeatUntilExit hIn hOut f (appendToLastFive c)
  where appendToLastFive a = (reverse . (:)a . take 4 . reverse) x

forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
