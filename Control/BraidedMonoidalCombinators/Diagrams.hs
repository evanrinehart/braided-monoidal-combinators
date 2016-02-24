{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Control.BraidedMonoidalCombinators.Diagrams where

-- Type definitions and utilities for diagrams

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
  Compose :: D r i j -> D r j k -> D r i k
  Filter :: D r '[E (Maybe a)] '[E a]
  Sum :: D r i j -> D r i' j' -> D r (i :+: i') (j :+: j')
  Trace :: D r (f a ': i) (f a ': j) -> D r i j 
  Request :: (r -> Resource a b c) -> D r '[E a] '[E b, V c]

instance Show (D r i j) where
  show d = case d of
    Empty -> "empty"
    Id -> "ident"
    Swap -> "swap"
    Copy -> "copy"
    Merge -> "merge"
    Null -> "hole"
    Never -> "never"
    Fmap _ -> "dmap _"
    Pure _ -> "always _"
    Appl -> "apply"
    Snap _ -> "snap _"
    Request _ -> "request _"
    Compose d1 d2 -> s1 ++ " >>> " ++ s2 where
      s1 = if size d1 > 1 && notCompose d1 then "(" ++ show d1 ++ ")" else show d1
      s2 = if size d2 > 1 && notCompose d2 then "(" ++ show d2 ++ ")" else show d2
    Filter -> "just"
    Sum d1 d2 -> s1 ++ " <> " ++ s2 where
      s1 = if size d1 > 1 && notSum d1 then "(" ++ show d1 ++ ")" else show d1
      s2 = if size d2 > 1 && notSum d2 then "(" ++ show d2 ++ ")" else show d2
    Trace d' -> if size d' > 1 then "trace(" ++ show d' ++ ")" else "trace " ++ show d'

notSum :: D r i j -> Bool
notSum (Sum _ _) = False
notSum _ = True

notCompose :: D r i j -> Bool
notCompose (Compose _ _) = False
notCompose _ = True

size :: D r i j -> Int
size d = case d of
  Empty -> 1
  Id -> 1
  Swap -> 1
  Copy -> 1
  Merge -> 1
  Null -> 1
  Never -> 1
  Fmap _ -> 1
  Pure _ -> 1
  Appl -> 1
  Snap _ -> 1
  Request _ -> 1
  Compose d1 d2 -> 2
  Filter -> 1
  Sum d1 d2 -> 2
  Trace d' -> 1

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
trace = Trace
just = Filter

snap' :: (a -> b -> c) -> D r '[V a, E b] '[E c]
snap' f = swap >>> snap (flip f)

apply' :: D r '[V a, V (a -> b)] '[V b]
apply' = swap >>> apply

data Storage a = Storage (IO a) (a -> IO ())

resourceFromStorage :: Storage a -> Resource a () a
resourceFromStorage (Storage get set) = Resource set get

resourceFromReq :: (a -> IO b) -> Resource a b ()
resourceFromReq req = Resource req (return ())

resourceFromQuery :: IO a -> Resource () () a
resourceFromQuery q = Resource (\_ -> return ()) q

var :: (r -> Storage a) -> D r '[E a] '[V a]
var getStore = Request (fmap resourceFromStorage getStore) >>> (hole <> ident)

request :: (r -> a -> IO b) -> D r '[E a] '[E b]
request getReq = Request (fmap resourceFromReq getReq) >>> (ident <> hole)

query :: (r -> IO a) -> D r '[] '[V a]
query getQ = never >>> Request (fmap resourceFromQuery getQ) >>> (hole <> ident)

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

-- non polymorphic versions of dmap might help with type errors
emap :: (a -> b) -> D r '[E a] '[E b]
emap = Fmap

vmap :: (a -> b) -> D r '[V a] '[V b]
vmap = Fmap

