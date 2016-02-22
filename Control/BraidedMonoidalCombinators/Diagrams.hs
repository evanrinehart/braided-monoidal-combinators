{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Control.BraidedMonoidalCombinators.Diagrams where

type family (a :: [*]) :+: (b :: [*]) :: [*] where
  '[] :+: xs = xs
  (y ': ys) :+: xs = y ': (ys :+: xs)
infixr 5 :+:

data E a
data V a

data Resource a b c = Resource (a -> IO b) (IO c)

data D :: * -> [*] -> [*] -> * where
  Empty :: D r '[] '[]
  Id :: D r '[f a] '[f a]
  Swap :: D r '[f a, g b] '[g b, f a]
  Copy :: D r '[f a] '[f a, f a]
  Merge :: D r '[E a, E a] '[E a]
  Null :: D r '[f a] '[]
  Never :: D r '[] '[E a]
  Fmap :: (a -> b) -> D r '[f a] '[f b]
  Pure :: a -> D r '[] '[V a]
  Appl :: D r '[V (a -> b), V a] '[V b]
  Snap :: (a -> b -> c) -> D r '[E a, V b] '[E c]
  Request :: (r -> Resource a b c) -> D r '[E a] '[E b, V c]
  Compose :: D r i j -> D r j k -> D r i k
  Sum :: D r i j -> D r i' j' -> D r (i :+: i') (j :+: j')
  Trace :: D r (f a ': i) (f a ': j) -> D r i j 

(<>) = Sum
(>>>) = Compose

ident = Id
empty = Empty
swap = Swap
copy = Copy
merge = Merge
hole = Null
never = Never
dmap = Fmap
always = Pure
apply = Appl
snap = Snap
request = Request
trace = Trace

data Storage a = Storage (IO a) (a -> IO ())

resourceFromStorage :: Storage a -> Resource a () a
resourceFromStorage (Storage get set) = Resource set get

var :: (r -> Storage a) -> D r '[E a] '[V a]
var getStore = request (fmap resourceFromStorage getStore) >>> (hole <> ident)
