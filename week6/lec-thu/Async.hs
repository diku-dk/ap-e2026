import Control.Concurrent (Chan, forkIO, newChan, threadDelay)
import Control.Exception (SomeException, catch, evaluate)
import GenServer

type Seconds = Int

data Status a
  = Value a
  | Timeout
  | Error String
  deriving (Show)

data Async a = Async (Server (Msg a))

data Msg a
  = MsgWait (ReplyChan (Status a))
  | MsgPoll (ReplyChan (Maybe (Status a)))
  | MsgTimeout
  | MsgPutVal a
  | MsgError String

serverLoopNoVal ::
  Chan (Msg a) ->
  [ReplyChan (Status a)] ->
  IO ()
serverLoopNoVal c waiters = do
  msg <- receive c
  case msg of
    MsgWait rc -> do
      serverLoopNoVal c (rc : waiters)
    MsgPoll rc -> do
      reply rc Nothing
      serverLoopNoVal c waiters
    MsgPutVal x -> do
      mapM_ (\rc -> reply rc (Value x)) waiters
      serverLoopVal c (Value x)
    MsgTimeout -> do
      mapM_ (\rc -> reply rc Timeout) waiters
      serverLoopVal c Timeout
    MsgError e -> do
      mapM_ (\rc -> reply rc (Error e)) waiters
      serverLoopVal c (Error e)

serverLoopVal ::
  Chan (Msg a) ->
  Status a ->
  IO ()
serverLoopVal c x = do
  msg <- receive c
  case msg of
    MsgWait rc -> do
      reply rc x
      serverLoopVal c x
    MsgPoll rc -> do
      reply rc (Just x)
      serverLoopVal c x
    MsgPutVal _ ->
      serverLoopVal c x
    MsgTimeout ->
      serverLoopVal c x
    MsgError _ ->
      serverLoopVal c x

async :: Seconds -> (b -> a) -> b -> IO (Async a)
async timeout f x = do
  s <- spawn $ \c -> do
    _ <- forkIO $ do
      let computation :: IO ()
          computation = do
            y <- evaluate $ f x
            send c $ MsgPutVal y
          handler :: SomeException -> IO ()
          handler e =
            send c (MsgError (show e))
      catch computation handler

    _ <- forkIO $ do
      threadDelay $ timeout * 1000000
      send c MsgTimeout

    serverLoopNoVal c []
  pure $ Async s

-- Block until result is available.
wait :: Async a -> IO (Status a)
wait (Async s) =
  requestReply s MsgWait

-- Check whether computation has finished. Should respond as quickly as
-- possible.
poll :: Async a -> IO (Maybe (Status a))
poll (Async s) =
  requestReply s MsgPoll

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n =
  if n < 0
    then error "fib: negative n"
    else fib (n - 1) + fib (n - 2)

waitAny :: [Async a] -> IO (Status a)
waitAny [] = error "empty list"
waitAny [x] = wait x
waitAny [x, y] = do
  c <- newChan
  forkIO $ do
    x' <- wait x
    send c x'
  forkIO $ do
    y' <- wait y
    send c y'
  receive c
waitAny (x : xs) = undefined -- TODO

demo :: IO ()
demo = do
  a <- async 10 fib (-1)
  there <- poll a
  putStrLn $ "poll returned: " ++ show there
  res <- wait a
  putStrLn "I got the result!"
  putStrLn "Here it is:"
  print res
