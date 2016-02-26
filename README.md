![alt text][logo]

# Microtubes

## Overview

These combinators let you construct low-level IO processing programs from
composable pieces.

Each diagram is either a primitive tube or a combination of smaller diagrams.
Each diagram has two sides, the source and the destination. Each side has
a list of port types which determine what kind of process or tube may be
connected in those locations.

`E` (event) ports expect an event source on the source side and an event
handler on the destination side.  `V` (view) ports expect a queryable resource
on the source side and a querying process on the destination. Haskell functions
can be promoted to the level of event or view transformer using `emap` or
`vmap` respectively.

The two main ways to combine diagrams is through composition `(>>>)` and
through concatenation `(<>)`. Composed diagrams must have compatible source
and destination, but any two diagrams can be concatenated. Composition has
the effect of hiding internal connections, and concatenation has the effect
of creating a larger diagram out of smaller ones.

Event and view transformers can interact directly through mutable variables.
The runtime dependence on variables is evident in the diagrams type. Variables
are an example of a resource. Other resources include external request actions
and queries that don't correspond to an external connection. Also views can be
accessed at the time of an event through the `snap` component.

Reordering, removing, duplicating, and merging tubes in the connection between
two diagrams is greatly simplified using the `[braid| i -> j |]` quasiquoter.

For stateful systems the `trace` operation loops the source back around to the
destination. The implementation rejects diagrams with invalid loops visible
to the outside world.

For a detailed slide show explaining the background and theory of this module,
see
[here](https://docs.google.com/presentation/d/1ZTHNJolxcUYrl-aPAMHfb5e0EQ_Fxpm8KYgbC1UHtt4/edit?usp=sharing).

The inspiration for these combinators comes from Pawel Sobocinski’s great blog
http://graphicallinearalgebra.net/ .

## Combinators

### `ident :: D '[f a] '[f a]`
The identity diagram is a simple connection and has no effect.

### `emap :: (a -> b) -> D '[E a] '[E b]`
### `vmap :: (a -> b) -> D '[V a] '[V b]`
Lift a function using either the E or V functor to get an event or
query transformer.

### `(>>>) :: D i j -> D j k -> D i k`
Compose two diagrams that have compatible ports

### `(<>) :: D i j -> D i' j' -> D (i :++: i') (j :++: j')`
Concat two diagrams. The resulting interface is the concatenation.

### `swap :: D '[f a, g b] '[g b, f a]`
Swaps two ports.

### `copy :: D '[f a] '[f a, f a]`
Copy a message going from source to two destinations, or merge two queries to
use the same source. There is no defined time ordering of copied messages
so don't rely on it.

### `merge :: D '[E a, E a] '[E a]`
Merge two message channels going right.

### `hole :: D '[f a] '[]`
Messages entering here will be lost. Useful for ignoring a port. Viewable sources
will never be queried by this.

### `never :: D '[] '[E a]`
A message source that never sends anything. Useful for ignoring a port.

### `always :: a -> D '[] '[V a]`
When queried this will always produce the same result.

### `apply  :: D '[V (a -> b), V a] '[V b]`
### `apply' :: D '[V a, V (a -> b)] '[V b]`
Merge two query results where one is a function.

### `just :: D '[E (Maybe a)] '[E a]`
Drop Nothings and only forward the unwrapped Justs.

### `snap  :: (a -> b -> c) -> D '[V a, E b] '[E c]`
### `snap' :: (a -> b -> c) -> D '[E b, V a] '[E c]`
### `snap_ :: (a -> b) -> D '[V a, E c] '[E b]`
### `snap_' :: (a -> b) -> D '[E c, V a] '[E b]`
On the event do a query and combine with a function.

### `trace :: D (f a ': i) (f a ': j) -> D i j`
Connect the first source and destination port with a loop. Certain bad loops
are not allowed and will be rejected before a program can run. Valid traces have
no effect in diagrams consisting only of push or only of pull connections.

### `var :: Storage a -> D '[E a] '[V a]`
A mutable variable whose storage must be provided at launch time.

### `query :: D '[V (IO a)] '[V a]`
An internal component for querying a resource.

### `request :: D '[E (IO a)] '[E a]`
Do an external request to a resource. The response will appear as a new
message.

### `empty :: D '[] '[]`
The empty diagram has no ports and so does nothing. It's the identity for
diagram concat. Many diagrams are equivalent to empty after you have
blocked all ports with holes, nevers, or traces.

## Routing QuasiQuoter

For anything but the simplest connections between components braiding and
merge / copying is incredibly tedious and complex. However there is a
quasiquoter which computes an efficient realization of any rearrangement:

```
> :set -XQuasiQuotes
> [braid| x y z -> z y x ]
(swap <> ident) >>> (ident <> swap) >>> (swap <> ident)

> [braid| x x -> x x]
merge >>> copy

> [braid| a b c d e f g h -> e f g h a b c d |]
(ident <> ident <> ident <> swap <> ident <> ident <> ident) >>>
(ident <> ident <> swap <> swap <> ident <> ident) >>>
(ident <> swap <> swap <> swap <> ident) >>>
(swap <> swap <> swap <> swap) >>>
(ident <> swap <> swap <> swap <> ident) >>>
(ident <> ident <> swap <> swap <> ident <> ident) >>>
(ident <> ident <> ident <> swap <> ident <> ident <> ident)
```

[logo]: https://raw.githubusercontent.com/evanrinehart/microtubes/master/image.png "Combinator Symbols"
