{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Control.Microtubes.Diagrams where

-- Type definitions and utilities for diagrams

type family (a :: [*]) :++: (b :: [*]) :: [*] where
  '[] :++: xs = xs
  (y ': ys) :++: xs = y ': (ys :++: xs)
infixr 5 :++:

data E a
data V a

data D :: [*] -> [*] -> * where
  Empty :: D '[] '[]
  Id :: D '[f a] '[f a]
  Swap :: D '[f a, g b] '[g b, f a]
  Copy :: D '[f a] '[f a, f a]
  Merge :: D '[E a, E a] '[E a]
  Null :: D '[f a] '[]
  Never :: D '[] '[E a]
  Fmap :: (a -> b) -> D '[f a] '[f b]
  Pure :: a -> D '[] '[V a]
  Apply :: D '[V (a -> b), V a] '[V b]
  Snap :: (a -> b -> c) -> D '[V a, E b] '[E c]
  Compose :: D i j -> D j k -> D i k
  Filter :: D '[E (Maybe a)] '[E a]
  Sum :: D i j -> D i' j' -> D (i :++: i') (j :++: j')
  Trace :: D (f a ': i) (f a ': j) -> D i j 
  Request :: D '[E (IO a)] '[E a]
  Query :: D '[V (IO a)] '[V a]

instance Show (D i j) where
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
    Apply -> "apply"
    Snap _ -> "snap _"
    Request -> "request"
    Query -> "query"
    Compose d1 d2 -> s1 ++ " >>> " ++ s2 where
      s1 = if size d1 > 1 && notCompose d1 then "(" ++ show d1 ++ ")" else show d1
      s2 = if size d2 > 1 && notCompose d2 then "(" ++ show d2 ++ ")" else show d2
    Filter -> "just"
    Sum d1 d2 -> s1 ++ " <> " ++ s2 where
      s1 = if size d1 > 1 && notSum d1 then "(" ++ show d1 ++ ")" else show d1
      s2 = if size d2 > 1 && notSum d2 then "(" ++ show d2 ++ ")" else show d2
    Trace d' -> if size d' > 1 then "trace(" ++ show d' ++ ")" else "trace " ++ show d'

notSum :: D i j -> Bool
notSum (Sum _ _) = False
notSum _ = True

notCompose :: D i j -> Bool
notCompose (Compose _ _) = False
notCompose _ = True

size :: D i j -> Int
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
  Apply -> 1
  Snap _ -> 1
  Request -> 1
  Compose d1 d2 -> 2
  Filter -> 1
  Sum d1 d2 -> 2
  Trace d' -> 1
  Query -> 1

(<>) :: D i j -> D i' j' -> D (i :++: i') (j :++: j')
(<>) = Sum

(>>>) :: D i j -> D j k -> D i k
(>>>) = Compose

ident :: D '[f a] '[f a]
ident = Id

empty :: D '[] '[]
empty = Empty

swap :: D '[f a, g b] '[g b, f a]
swap = Swap

copy :: D '[f a] '[f a, f a]
copy = Copy

merge :: D '[E a, E a] '[E a]
merge = Merge

hole :: D '[f a] '[]
hole = Null

never :: D '[] '[E a]
never = Never

dmap :: (a -> b) -> D '[f a] '[f b]
dmap = Fmap

always :: a -> D '[] '[V a]
always = Pure

apply :: D '[V (a -> b), V a] '[V b]
apply = Apply

snap :: (a -> b -> c) -> D '[V a, E b] '[E c]
snap = Snap

trace :: D (f a ': i) (f a ': j) -> D i j
trace = Trace

just :: D '[E (Maybe a)] '[E a]
just = Filter

snap' :: (a -> b -> c) -> D '[E b, V a] '[E c]
snap' f = swap >>> snap f

snap_ :: (a -> b) -> D '[V a, E c] '[E b]
snap_ f = snap (\x _ -> f x)

snap_' :: (a -> b) -> D '[E c, V a] '[E b]
snap_' f = snap' (\x _ -> f x)

apply' :: D '[V a, V (a -> b)] '[V b]
apply' = swap >>> apply

var :: IO a -> (a -> IO ()) -> D '[E a] '[V a]
var get set = emap set >>> request >>> hole >>> always get >>> query

request :: D '[E (IO a)] '[E a]
request = Request 

query :: D '[V (IO a)] '[V a]
query = Query

swap3 :: D '[f a, g b, h c] '[h c, g b, f a]
swap3 = (ident <> swap) >>> (swap <> ident) >>> (ident <> swap)

split :: D '[E (a,b)] '[E a, E b]
split = copy >>> (dmap fst <> dmap snd)

liftA2 :: (a -> b -> c) -> D '[V a, V b] '[V c]
liftA2 f = (always f <> ident <> ident) >>> (apply <> ident) >>> apply

liftA3 :: (a -> b -> c -> d) -> D '[V a, V b, V c] '[V d]
liftA3 f =
  (always f <> ident <> ident <> ident) >>>
  (apply <> ident <> ident) >>>
  (apply <> ident) >>>
  apply

liftA4 :: (a -> b -> c -> d -> e) -> D '[V a, V b, V c, V d] '[V e]
liftA4 f =
  (always f <> ident <> ident <> ident <> ident) >>>
  (apply <> ident <> ident <> ident) >>>
  (apply <> ident <> ident) >>>
  (apply <> ident) >>>
  apply

-- non polymorphic versions of dmap might help with type errors
emap :: (a -> b) -> D '[E a] '[E b]
emap = Fmap

vmap :: (a -> b) -> D '[V a] '[V b]
vmap = Fmap

