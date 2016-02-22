module Control.BraidedMonoidalCombinators.Swarm where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad

data TerminationStatus =
  WorkerStopped String |
  WorkerError String SomeException deriving (Show)

fromEither :: (String, Either SomeException ()) -> TerminationStatus
fromEither (name, Left e) = WorkerError name e
fromEither (name, Right _) = WorkerStopped name

-- spawn a bunch of threads which all get killed if any one of them dies
-- the returned IO action waits for this event and returns the exception
forkSwarm :: [(String, IO ())] -> IO (IO TerminationStatus)
forkSwarm workers = do
  sigTV <- newEmptyTMVarIO
  tidsTV <- newTVarIO []
  let append x = atomically $ modifyTVar tidsTV (x:)
  forM_ workers $ \(name, w) -> do
    tid <- forkFinally w $ \e -> do
      atomically (putTMVar sigTV (name, e))
    append tid 
  resultMV <- newEmptyTMVarIO
  forkIO $ do
    whoWhat <- atomically (takeTMVar sigTV)
    tids <- atomically (readTVar tidsTV)
    forM_ tids killThread
    atomically (putTMVar resultMV (fromEither whoWhat))
  return (atomically (takeTMVar resultMV))
