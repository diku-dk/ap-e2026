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

data Msg
  = MsgInc
      Int
      (Chan Int) -- reply channel
  | MsgSet Int

data IncServer = IncServer (Chan Msg)

newServer :: IO IncServer
newServer = do
  c <- newChan
  let threadLoop state = do
        msg <- readChan c
        case msg of
          MsgInc x rc -> do
            writeChan rc state
            threadLoop $ state + x
          MsgSet x ->
            threadLoop x
  forkIO $ threadLoop 0
  pure $ IncServer c

-- Example of RPC operation
serverInc :: Int -> IncServer -> IO Int
serverInc x (IncServer c) = do
  rc <- newChan
  let msg = MksgInc x rc
  writeChan c msg
  readChan rc

-- Example of asynchronous operation
serverSet :: Int -> IncServer -> IO ()
serverSet x (IncServer c) = do
  writeChan c $ MsgSet x

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
