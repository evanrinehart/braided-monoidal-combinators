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

empty :: D r '[] '[]
empty = Empty

swap :: D r '[f a, g b] '[g b, f a]
swap = Swap

copy :: D r '[f a] '[f a, f a]
copy = Copy

merge :: D r '[E a, E a] '[E a]
merge = Merge

hole :: D r '[f a] '[]
hole = Null

never :: D r '[] '[E a]
never = Never

dmap :: (a -> b) -> D r '[f a] '[f b]
dmap = Fmap

always :: a -> D r '[] '[V a]
always = Pure

apply :: D r '[V (a -> b), V a] '[V b]
apply = Appl

snap :: (a -> b -> c) -> D r '[E a, V b] '[E c]
snap = Snap

request :: (r -> Resource a b c) -> D r '[E a] '[E b, V c]
request = Request

trace :: D r (f a ': i) (f a ': j) -> D r i j 
trace = Trace

