{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
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

import Control.Microtubes.Diagrams
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
doSnap f ioy ioz = \x -> do
  y <- ioy
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
  VWrk String (Worker (Query Any)) -- concrete worker, doesn't appear in algorithm

instance Show Crumb where
  show cr = case cr of
    DummyE -> "DummyE"
    DummyV -> "DummyV"
    Cmd _ -> "Cmd _"
    Qry _ -> "Qry _"
    Unknown -> "Unknown"
    EWrk name _ -> "EWrk " ++ name
    VWrk name _ -> "VWrk " ++ name

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
zipWorkerE (_:xs) (_:ys) = zipWorkerE xs ys

zipWorkerV :: [Crumb] -> [Crumb] -> [(String, IO ())]
zipWorkerV [] [] = []
zipWorkerV (VWrk _ _:_) (Unknown:_) = error "there is a query leading to an invalid loop"
zipWorkerV (VWrk name w:xs) (Qry q:ys) = (name, w q) : zipWorkerV xs ys
zipWorkerV (_:xs) (_:ys) = zipWorkerV xs ys

data Run :: [*] -> [*] -> * where
  MkRun :: forall r i j i' j'. (r, D r i j) -> [Crumb] -> [Crumb] -> Run i' j'

begin :: r -> D r i j -> Run i j
begin r dia = MkRun (r,dia) [] []

equipCommandWorker :: Worker (Command a) -> Run (E a ': i) j -> Run i j
equipCommandWorker w (MkRun b cs1 cs2) = MkRun b (cs1 ++ [EWrk "unknown command worker" (castCmdWorker w)]) cs2

equipQueryWorker :: Worker (Query a) -> Run i (V a ': j) -> Run i j
equipQueryWorker w (MkRun b cs1 cs2) = MkRun b (cs1 ++ [VWrk "unknown query worker" (castQryWorker w)]) cs2

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
fork (MkRun (r,c) ins outs) = do
  let lCrumbs = dummyify ins
  let rCrumbs = dummyify outs
  putStrLn "provided:"
  print lCrumbs
  print rCrumbs
  tidsRef <- newIORef []
  let append x = modifyIORef tidsRef (x:)
  let (commands, queries, _, _, reqWorkers) = applyDiagram c r lCrumbs rCrumbs
  putStrLn "transformed:"
  print commands
  print queries
  let workerEs = zipWorkerE ins commands
  let workerVs = zipWorkerV outs queries
--  workerReqs <- forM mkWorkerReqs $ \mk -> do
--    tchan <- newTChanIO
--    return (mk tchan)
  forkSwarm (map (\x -> ("request worker", x)) reqWorkers ++ workerEs ++ workerVs)

-- This is morally reprehensible!
unsafeTChanOutOfNowhere :: () -> TChan Any
unsafeTChanOutOfNowhere () = unsafePerformIO newTChanIO

setupMkReq :: Command Any -> (Any -> IO Any) -> TChan Any -> (Command Any, IO ())
setupMkReq cmd req tchan = (cmd', worker) where
  cmd' x = atomically (writeTChan tchan x)
  worker = reqWorker cmd req tchan

reqWorker :: (Any -> IO ()) -> (Any -> IO Any) -> TChan Any -> IO ()
reqWorker exec req tchan = forever $ do
  x <- atomically (readTChan tchan)
  y <- req x
  exec y

applyDiagram :: D r i j -> r -> [Crumb] -> [Crumb] -> ([Crumb], [Crumb], [Crumb], [Crumb], [IO ()])
applyDiagram c r ins outs = case c of
  Empty -> ([], [], ins, outs, [])
  Id -> case (ins, outs) of
    ~(x:ins', y:outs') -> ([y], [x], ins', outs', [])
  Swap -> case (ins, outs) of
    ~(x1:x2:ins', y1:y2:outs') -> ([y2,y1], [x2,x1], ins', outs', [])
  Copy -> case (ins, outs) of
    ~(x:ins', y1:y2:outs') -> ([x'], [y1',y2'], ins', outs', []) where
      (x',y1',y2') = case (x,y1,y2) of
        (_, Cmd c1, Cmd c2) -> (Cmd (copyCommand c1 c2), DummyE, DummyE)
        (Qry q, _, _) -> (DummyV, Qry q, Qry q)
        (_, _, _) -> (Unknown, Unknown, Unknown)
  Merge -> case (ins, outs) of
    ~(x1:x2:ins', y:outs') -> ([y, y], [z], ins', outs', []) where
      z = case (x1,x2) of
        (Unknown, _) -> Unknown
        (_, Unknown) -> Unknown
        _ -> DummyE
  Null -> case ins of
    ~(x:ins') -> ([x'], [], ins', outs, []) where
      x' = case x of
        DummyE -> Cmd noop
        (Qry _) -> DummyV
        _ -> Unknown
  Never -> case outs of
    ~(y:outs') -> ([], [y'], ins, outs', []) where
      y' = case y of
        (Cmd _) -> DummyE
        _ -> Unknown
  Fmap f -> case (ins, outs) of
    ~(x:ins', y:outs') -> ([x'], [y'], ins', outs', []) where
      (x',y') = case (x,y) of
        (_, Cmd cmd) -> (Cmd (mapCommand f cmd), DummyE)
        (Qry q, _) -> (DummyV, Qry (mapQuery f q))
        (_,_) -> (Unknown, Unknown)
  Pure v -> case outs of
    ~(_:outs') -> ([], [Qry (pureQuery v)], ins, outs', [])
  Appl -> case (ins, outs) of
    ~(x1:x2:ins', y:outs') -> ([x1',x2'], [y'], ins', outs', []) where
      (x1', x2', y') = case (x1,x2,y) of
        (Qry q1, Qry q2, _) -> (DummyV, DummyV, Qry (applQueries q1 q2))
        (_,_,_) -> (Unknown, Unknown, Unknown)
  Snap f -> case (ins, outs) of
    ~(x1:x2:ins', y:outs') -> ([x1',x2'], [y'], ins', outs', []) where
      (x1', x2', y') = case (x1,x2,y) of
        (_, Qry q, Cmd cmd) -> (Cmd (doSnap f q cmd), DummyV, DummyE)
        (_,_,_) -> (Unknown, Unknown, Unknown)
  Request getResource -> let (Resource exec view) = getResource r in case (ins, outs) of
    ~(x:ins', y1:y2:outs') -> ([x'], [y1',y2'], ins', outs', mk) where
      (x', y1', y2', mk) = case (x, y1, y2) of
        (_, Cmd cmd, _) -> let (cmd', mkreq) = setupMkReq cmd (unsafeCoerce exec) (unsafeTChanOutOfNowhere ()) in (Cmd cmd', DummyE, Qry (viewCosnap view), [mkreq])
        (_, _, _) -> (Unknown, Unknown, Unknown, [])
  Compose c1 c2 -> (leftCrumbs, rightCrumbs, ins', outs', mkr1++mkr2) where
    (leftCrumbs, middleLeft, ins', _, mkr1) = applyDiagram c1 r ins middleRight
    (middleRight, rightCrumbs, _, outs', mkr2) = applyDiagram c2 r middleLeft outs
  Filter -> case (ins, outs) of
    ~(x:ins', y:outs') -> ([x'], [y'], ins', outs', []) where
      (x', y') = case (x,y) of
        (_, Cmd cmd) -> (Cmd (filterCommand cmd), DummyE)
        (_, _) -> (Unknown, Unknown)
  Sum c1 c2 -> (x1++x2, y1++y2, ins'', outs'', mkr1++mkr2) where
    (x1, y1, ins', outs', mkr1) = applyDiagram c1 r ins outs
    (x2, y2, ins'', outs'', mkr2) = applyDiagram c2 r ins' outs'
  Trace c' -> 
    let (x:xs, y:ys, ins', outs',mkreqs) = applyDiagram c' r (Unknown:ins) (Unknown:outs) in
    let drop1 (x:xs, y:ys, ins, outs, mkreqs) = (xs,ys,ins,outs,mkreqs) in
    case (x,y) of
      (orig@(Cmd cmd), _) -> drop1 (applyDiagram c' r (DummyE:ins) (orig:outs))
      (_, orig@(Qry q)) -> drop1 (applyDiagram c' r (orig:ins) (DummyV:outs))
      _ -> (xs,ys,ins',outs',mkreqs)

-- experimental component which runs a sub program which can be swapped out
-- data PortParts i j = PortParts [TMVar Any] [TMVar Any]
-- Port :: (r -> PortParts i' j') -> (r -> BundleStorage i' j') -> DefaultPorts i' j' -> C r (E (Bundle i' j') ': i') (E TerminationStatus ': j')
