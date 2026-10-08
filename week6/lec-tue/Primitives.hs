import Control.Concurrent
  ( Chan,
    ThreadId,
    forkIO,
    newChan,
    readChan,
    threadDelay,
    writeChan,
  )

messageExample :: IO ()
messageExample = do
  c <- newChan
  forkIO $ writeChan c "42"
  forkIO $ writeChan c "43"
  let threadLoop = do
        x <- readChan c
        putStrLn $ "Received: " ++ x
        threadLoop
  forkIO threadLoop
  pure ()

-- Violation of the single-reader principle!
messageExample2 :: IO ()
messageExample2 = do
  c <- newChan
  forkIO $ writeChan c "42"
  forkIO $ writeChan c "43"
  let threadLoop t = do
        x <- readChan c
        putStrLn $ t ++ " received: " ++ x
        threadLoop t
  forkIO (threadLoop "A")
  forkIO (threadLoop "B")
  pure ()
