module IncServerGen
  ( IncServer,
    newServer,
    serverInc,
    serverSet,
  )
where

import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )
import Control.Monad (forM)
import GenServer

data Msg
  = MsgInc Int (ReplyChan Int)
  | MsgSet Int

data IncServer = IncServer (Server Msg)

newServer :: IO IncServer
newServer = do
  let serverLoop state c = do
        msg <- receive c
        case msg of
          MsgSet x -> serverLoop x c
          MsgInc x rc -> do
            reply rc x
            serverLoop (state + x) c
  s <- spawn $ serverLoop 0
  pure $ IncServer s

-- Example of RPC operation
serverInc :: Int -> IncServer -> IO Int
serverInc x (IncServer s) =
  requestReply s $ MsgInc x

-- Example of asynchronous operation
serverSet :: Int -> IncServer -> IO ()
serverSet x (IncServer s) =
  sendTo s $ MsgSet x

usage :: IO ()
usage = do
  s <- newServer

  forM [1 .. 10] $ \i ->
    forkIO $ do
      serverInc i s
      pure ()

  serverSet 0 s

  r <- serverInc 0 s
  putStrLn $ "response: " ++ show r
