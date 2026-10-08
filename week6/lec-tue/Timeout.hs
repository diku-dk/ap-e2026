import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )

type Timeout = Int

data Result a = Timeout | Result a
  deriving (Show)

readWithTimeout ::
  Chan a ->
  Timeout ->
  IO (Result a)
readWithTimeout c t = do
  -- tc :: Chan (Maybe a)
  tc <- newChan
  forkIO $ do
    threadDelay t
    writeChan tc Timeout
  forkIO $ do
    x <- readChan c
    writeChan tc $ Result x
  readChan tc

demo :: IO ()
demo = do
  c <- newChan
  forkIO $ do
    threadDelay 1000000000
    writeChan c 123
  x <- readWithTimeout c 1000000
  print x
