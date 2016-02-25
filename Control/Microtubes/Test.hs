{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Control.Microtubes.Test where

import Data.Proxy
import Control.Concurrent
import Test.QuickCheck
import Control.Monad 

import Control.Microtubes.Diagrams
import Control.Microtubes.Runner

class TestInput (a :: [*]) where
  equipTestIn :: Run a j -> Run '[] j

class TestOutput (a :: [*]) where
  equipTestOut :: Run i a -> Run i '[]

instance TestInput '[] where
  equipTestIn r = r

instance TestOutput '[] where
  equipTestOut r = r

instance (Arbitrary a, TestInput ts) => TestInput (E a ': ts) where
  equipTestIn = equipTestIn . equipCommandWorker (\cmd -> forever (threadDelay 1000000 >> generate arbitrary >>= cmd))

instance (Arbitrary a, TestInput ts) => TestInput (V a ': ts) where
  equipTestIn = equipTestIn . equipQuery (generate arbitrary)

instance (Show a, TestOutput ts) => TestOutput (E a ': ts) where
  equipTestOut = equipTestOut . equipCommand print

instance (Show a, TestOutput ts) => TestOutput (V a ': ts) where
  equipTestOut = equipTestOut . equipQueryWorker (\q -> forever (threadDelay 800000 >> q >>= print))

test :: (TestInput i, TestOutput j) => D i j -> IO ()
test d = run $ (equipTestIn . equipTestOut) (begin d)
