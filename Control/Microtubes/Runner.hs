{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
module Control.Microtubes.Runner where

import GHC.Prim
import Data.Either
import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Functor
import Data.Char
import Unsafe.Coerce
import System.IO.Unsafe
import Control.Exception
import Control.Concurrent.STM

import Debug.Trace

import Control.Microtubes.Diagrams
import qualified Control.Microtubes.Diagrams as D
import Control.Microtubes.Swarm

{-
What the runner does:
If you give it a diagram, proper resources and fully equip it with
workers, commands, and queriable sources, then it will launch threads to
execute everything. If any worker thread suffers an exception, everything
shuts down. The caller can be provided with a join command to get the
exit status.

How it works:
It treats the diagram as a function which simultaneously transforms
provided commands into commands usable by workers, and provided queries
into queries viewable by query workers. The final queries and commands are
provided to the workers, which run in their own threads.

Data processing in this module is highly unsafe! However the diagram
language stops Any values that are not compatible from interacting.
-}

type Query a = IO a
type Command a = a -> IO ()
type Worker r = r -> IO ()

mapQuery :: (a -> b) -> Query Any -> Query Any
mapQuery f io = do
  x <- io
  let y = f (unsafeCoerce x)
  return (unsafeCoerce y)

pureQuery :: a -> Query Any
pureQuery x = do
  return (unsafeCoerce x)

applQueries :: Query Any -> Query Any -> Query Any
applQueries iof iox = do
  f <- iof
  x <- iox
  let g = unsafeCoerce f :: a -> b
  let y = unsafeCoerce x :: a
  let z = unsafeCoerce (g y) :: Any
  return z

mapCommand :: (a -> b) -> Command Any -> Command Any
mapCommand f io = \x -> do
  let y = f (unsafeCoerce x)
  io (unsafeCoerce y)

doSnap :: (a -> b -> c) -> Query Any -> Command Any -> Command Any
doSnap f iox ioz = \y -> do
  x <- iox
  let z = f (unsafeCoerce x) (unsafeCoerce y)
  ioz (unsafeCoerce z)

doCosnap :: (a -> IO b) -> Command Any -> Command Any
doCosnap io exec = \x -> do
  y <- io (unsafeCoerce x)
  exec (unsafeCoerce y)

viewCosnap :: IO c -> Query Any
viewCosnap io = do
  x <- io
  return (unsafeCoerce x)

copyCommand :: Command Any -> Command Any -> Command Any
copyCommand io1 io2 = \x -> do
  io1 x
  io2 x

-- the argument to this is a Query (Query Any)
doQuery :: Query Any -> Query Any
doQuery q = do
  let ioio = unsafeCoerce q :: Query (Query Any)
  io <- ioio
  io

filterCommand :: Command Any -> Command Any
filterCommand io = \x -> do
  let mv = unsafeCoerce x :: Maybe Any
  case mv of
    Nothing -> return ()
    Just v -> io v

noop :: a -> IO ()
noop _ = return ()

castQuery :: Query a -> Query Any
castQuery io = unsafeCoerce io

castCommand :: Command a -> Command Any
castCommand io = unsafeCoerce io

castCmdWorker :: Worker (Command a) -> Worker (Command Any)
castCmdWorker w = unsafeCoerce w

castQryWorker :: Worker (Query a) -> Worker (Query Any)
castQryWorker w = unsafeCoerce w

data Crumb =
  DummyE | -- space for a EWrk
  DummyV | -- space for a VWrk
  Cmd (Command Any) | -- a concrete, non-looping command
  Qry (Query Any) | -- a concrete, non-looping query
  Unknown | -- an unknown line that might loop
  EWrk String (Worker (Command Any)) | -- concrete worker, doesn't appear in algorithm
  VWrk String (Worker (Query Any)) | -- concrete worker, doesn't appear in algorithm
  BlackHole -- will never contact you, can be sent anything safely, querying would be an error

instance Show Crumb where
  show cr = case cr of
    DummyE -> "DummyE"
    DummyV -> "DummyV"
    Cmd _ -> "Cmd _"
    Qry _ -> "Qry _"
    Unknown -> "Unknown"
    EWrk name _ -> "EWrk " ++ name
    VWrk name _ -> "VWrk " ++ name
    BlackHole -> "BlackHole"

dummy :: (String, Worker a)
dummy = ("dummy", \_ -> forever (threadDelay 100000000))

dummyify :: [Crumb] -> [Crumb]
dummyify [] = []
dummyify (x:xs) = case x of
  EWrk _ _ -> DummyE : dummyify xs
  VWrk _ _ -> DummyV : dummyify xs
  _ -> x : dummyify xs

zipWorkerE :: [Crumb] -> [Crumb] -> [(String, IO ())]
zipWorkerE [] [] = []
zipWorkerE ((EWrk _ _):_) (Unknown:_) = error "there is a command leading to an invalid loop"
zipWorkerE ((EWrk name w):xs) (Cmd c:ys) = (name, w c) : zipWorkerE xs ys
zipWorkerE ((EWrk name w):xs) (BlackHole:ys) = (name, w noop) : zipWorkerE xs ys
zipWorkerE (_:xs) (_:ys) = zipWorkerE xs ys

zipWorkerV :: [Crumb] -> [Crumb] -> [(String, IO ())]
zipWorkerV [] [] = []
zipWorkerV (VWrk _ _:_) (Unknown:_) = error "there is a query leading to an invalid loop"
zipWorkerV (VWrk name w:xs) (Qry q:ys) = (name, w q) : zipWorkerV xs ys
zipWorkerV (VWrk name w:xs) (BlackHole:_) = error "must be a bug: query worker paired with a blackhole"
zipWorkerV (_:xs) (_:ys) = zipWorkerV xs ys

data Run :: [*] -> [*] -> * where
  MkRun :: forall i j i' j'. D i j -> [Crumb] -> [Crumb] -> Run i' j'

begin :: D i j -> Run i j
begin dia = MkRun dia [] []

equipCommandWorker :: Worker (Command a) -> Run (E a ': i) j -> Run i j
equipCommandWorker w (MkRun b cs1 cs2) = MkRun b (cs1 ++ [EWrk "unknown command worker" (castCmdWorker w)]) cs2

equipQueryWorker :: Worker (Query a) -> Run i (V a ': j) -> Run i j
equipQueryWorker w (MkRun b cs1 cs2) = MkRun b cs1 (cs2 ++ [VWrk "unknown query worker" (castQryWorker w)])

equipQuery :: Query a -> Run (V a ': i) j -> Run i j
equipQuery q (MkRun b cs1 cs2) = MkRun b (cs1 ++ [Qry (castQuery q)]) cs2

equipCommand :: Command a -> Run i (E a ': j) -> Run i j
equipCommand c (MkRun b cs1 cs2) = MkRun b cs1 (cs2 ++ [Cmd (castCommand c)])

run :: Run '[] '[] -> IO ()
run r = do
  wait <- fork r
  status <- wait
  case status of
    WorkerStopped _ -> return ()
    WorkerError _ e -> throwIO e

fork :: Run '[] '[] -> IO (IO TerminationStatus)
fork (MkRun d ins outs) = do
  let lCrumbs = dummyify ins
  let rCrumbs = dummyify outs
  putStrLn "provided:"
  print lCrumbs
  print rCrumbs
  tidsRef <- newIORef []
  let append x = modifyIORef tidsRef (x:)
  let (commands, queries, _, _, reqWorkers) = applyDiagram d lCrumbs rCrumbs
  print commands
  print queries
  let workerEs = zipWorkerE ins commands
  let workerVs = zipWorkerV outs queries
--  workerReqs <- forM mkWorkerReqs $ \mk -> do
--    tchan <- newTChanIO
--    return (mk tchan)
  forkSwarm (map (\x -> ("request worker", x)) reqWorkers ++ workerEs ++ workerVs)

-- This is morally reprehensible!
unsafeTChanOutOfNowhere :: () -> TChan (IO Any)
unsafeTChanOutOfNowhere () = unsafePerformIO newTChanIO

setupMkReq :: Command Any -> TChan (IO Any) -> (Command (IO Any), IO ())
setupMkReq cmd tchan = (cmd', worker) where
  cmd' x = atomically (writeTChan tchan x)
  worker = reqWorker cmd tchan

reqWorker :: (Any -> IO ()) -> TChan (IO Any) -> IO ()
reqWorker exec tchan = forever $ do
  io <- atomically (readTChan tchan)
  y <- io
  exec y

applyDiagram :: D i j -> [Crumb] -> [Crumb] -> ([Crumb], [Crumb], [Crumb], [Crumb], [IO ()])
applyDiagram c ins outs = case c of
  Empty -> ([], [], ins, outs, [])
  Id -> case (ins, outs) of
    ~(x:ins', y:outs') -> ([y], [x], ins', outs', [])
  Swap -> case (ins, outs) of
    ~(x1:x2:ins', y1:y2:outs') -> ([y2,y1], [x2,x1], ins', outs', [])
  Copy -> case (ins, outs) of
    ~(x:ins', y1:y2:outs') -> ([x'], [y1',y2'], ins', outs', []) where
        (y1', y2') = case x of
          Qry q -> (Qry q, Qry q)
          _ -> (Unknown, Unknown)
        x' = case (y1, y2) of
          (Cmd c1, Cmd c2) -> Cmd (copyCommand c1 c2)
          (BlackHole, Cmd c2) -> Cmd c2
          (Cmd c1, BlackHole) -> Cmd c1
          (BlackHole, BlackHole) -> BlackHole
          _ -> Unknown
  Merge -> case (ins, outs) of
    ~(x1:x2:ins', y:outs') -> ([y, y], [z], ins', outs', []) where
      z = case (x1,x2) of
        (Unknown, _) -> Unknown
        (_, Unknown) -> Unknown
        _ -> DummyE
  Null -> case ins of
    ~(_:ins') -> ([BlackHole], [], ins', outs, [])
  Never -> case outs of
    ~(_:outs') -> ([], [DummyE], ins, outs', [])
  Fmap f -> case (ins, outs) of
    ~(x:ins', y:outs') -> ([x'], [y'], ins', outs', []) where
      x' = case y of
        Cmd cmd -> Cmd (mapCommand f cmd)
        BlackHole -> BlackHole
        _ -> Unknown
      y' = case x of
        Qry q -> Qry (mapQuery f q)
        _ -> Unknown
  Pure v -> case outs of
     ~(_:outs') -> ([], [Qry (pureQuery v)], ins, outs', [])
  Appl -> case (ins, outs) of
    ~(x1:x2:ins', y:outs') -> ([x1',x2'], [y'], ins', outs', []) where
      (x1', x2', y') = case (x1,x2) of
        (Qry q1, Qry q2) -> (DummyV, DummyV, Qry (applQueries q1 q2))
        (_,_) -> (Unknown, Unknown, Unknown)
  Snap f -> case (ins, outs) of
    ~(x:_:ins', y:outs') -> ([DummyV,x'], [DummyE], ins', outs', []) where
      x' = case (x,y) of
        (Qry q, Cmd cmd) -> Cmd (doSnap f q cmd)
        (Qry q, BlackHole) -> Cmd (doSnap f q noop)
        (_,_) -> Unknown
  Request -> case (ins, outs) of
    ~(_:ins', y:outs') -> ([x'], [DummyE], ins', outs', mk) where
      (x', mk) = case y of
        Cmd cmd -> let (cmd', mkreq) = setupMkReq cmd (unsafeTChanOutOfNowhere ()) in (Cmd (unsafeCoerce cmd'), [mkreq])
        BlackHole -> let (cmd', mkreq) = setupMkReq noop (unsafeTChanOutOfNowhere ()) in (Cmd (unsafeCoerce cmd'), [mkreq])
        _ -> (Unknown, [])
  Query -> case (ins, outs) of
    ~(x:ins', _:outs') -> ([DummyV], [y'], ins', outs', []) where
       y' = case x of
          Qry q -> Qry (doQuery q)
          _ -> Unknown
  Compose c1 c2 -> (leftCrumbs, rightCrumbs, ins', outs', mkr1++mkr2) where
    (leftCrumbs, middleLeft, ins', _, mkr1) = applyDiagram c1 ins middleRight
    (middleRight, rightCrumbs, _, outs', mkr2) = applyDiagram c2 middleLeft outs
  Filter -> case (ins, outs) of
    ~(_:ins', y:outs') -> ([x'], [DummyE], ins', outs', []) where
      x' = case y of
        Cmd cmd -> Cmd (filterCommand cmd)
        BlackHole -> BlackHole
        _ -> Unknown
  Sum c1 c2 -> (x1++x2, y1++y2, ins'', outs'', mkr1++mkr2) where
    (x1, y1, ins', outs', mkr1) = applyDiagram c1 ins outs
    (x2, y2, ins'', outs'', mkr2) = applyDiagram c2 ins' outs'
  Trace c' -> 
    let (x:xs, y:ys, ins', outs',mkreqs) = applyDiagram c' (Unknown:ins) (Unknown:outs) in
    let drop1 (x:xs, y:ys, ins, outs, mkreqs) = (xs,ys,ins,outs,mkreqs) in
    case (x,y) of
      (orig@(Cmd cmd), _) -> drop1 (applyDiagram c' (DummyE:ins) (orig:outs))
      (BlackHole, _) -> drop1 (applyDiagram c' (DummyE:ins) (BlackHole:outs))
      (_, orig@(Qry q)) -> drop1 (applyDiagram c' (orig:ins) (DummyV:outs))
      _ -> (xs,ys,ins',outs',mkreqs)

debug_ :: Show s => s -> s
debug_ x = Debug.Trace.trace ("debug: "++ show x) x

-- experimental component which runs a sub program which can be swapped out
-- data PortParts i j = PortParts [TMVar Any] [TMVar Any]
-- Port :: (r -> PortParts i' j') -> (r -> BundleStorage i' j') -> DefaultPorts i' j' -> C r (E (Bundle i' j') ': i') (E TerminationStatus ': j')

