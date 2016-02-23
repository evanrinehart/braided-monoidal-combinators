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
  Filter :: D r '[E (Maybe a)] '[E a]
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
just = Filter

snap' :: (a -> b -> c) -> D r '[V a, E b] '[E c]
snap' f = swap >>> snap (flip f)

apply' :: D r '[V a, V (a -> b)] '[V b]
apply' = swap >>> apply

request' :: (r -> Resource a b c) -> D r '[E a] '[V c, E b]
request' getR = request getR >>> swap

data Storage a = Storage (IO a) (a -> IO ())

resourceFromStorage :: Storage a -> Resource a () a
resourceFromStorage (Storage get set) = Resource set get

var :: (r -> Storage a) -> D r '[E a] '[V a]
var getStore = request (fmap resourceFromStorage getStore) >>> (hole <> ident)

swap3 :: D r '[f a, g b, h c] '[h c, g b, f a]
swap3 = (ident <> swap) >>> (swap <> ident) >>> (ident <> swap)

split :: D r '[E (a,b)] '[E a, E b]
split = copy >>> (dmap fst <> dmap snd)

liftA2 :: (a -> b -> c) -> D r '[V a, V b] '[V c]
liftA2 f = (always f <> ident <> ident) >>> (apply <> ident) >>> apply

liftA3 :: (a -> b -> c -> d) -> D r '[V a, V b, V c] '[V d]
liftA3 f =
  (always f <> ident <> ident <> ident) >>>
  (apply <> ident <> ident) >>>
  (apply <> ident) >>>
  apply

liftA4 :: (a -> b -> c -> d -> e) -> D r '[V a, V b, V c, V d] '[V e]
liftA4 f =
  (always f <> ident <> ident <> ident <> ident) >>>
  (apply <> ident <> ident <> ident) >>>
  (apply <> ident <> ident) >>>
  (apply <> ident) >>>
  apply
