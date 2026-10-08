module GenServer
  ( Server,
    send,
    receive,
    spawn,
    sendTo,
    ReplyChan,
    reply,
    requestReply,
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

data Server msg = Server ThreadId (Chan msg)

spawn :: (Chan msg -> IO ()) -> IO (Server msg)
spawn f = do
  c <- newChan
  tid <- forkIO $ f c
  pure $ Server tid c

send :: Chan msg -> msg -> IO ()
send = writeChan

receive :: Chan msg -> IO msg
receive = readChan

-- asynchronous
sendTo :: Server msg -> msg -> IO ()
sendTo (Server _ c) msg = send c msg

data ReplyChan a = ReplyChan (Chan a)

reply :: ReplyChan a -> a -> IO ()
reply (ReplyChan c) x = send c x

requestReply ::
  Server msg ->
  (ReplyChan a -> msg) ->
  IO a
requestReply server mkMsg = do
  rc <- newChan
  let msg = mkMsg $ ReplyChan rc
  sendTo server msg
  receive rc
