![alt text][logo]

# Braided Monoidal Combinators

These combinators let you construct low-level IO processing programs from
composable pieces. The underlying data type is a category where the objects
are lists of Haskell types and the morphisms are diagrams that connect one
list of types to another.

For a detailed slide show explaining the background and theory of this
module, see [here](https://docs.google.com/presentation/d/1ZTHNJolxcUYrl-aPAMHfb5e0EQ_Fxpm8KYgbC1UHtt4/edit?usp=sharing). As you will no doubt discover, braided monoidal combinators are a mix
of fun and serious.

The inspiration for this library comes from [http://graphicallinearalgebra.net/]
which is an awesome blog, check it out.

Two characteristics of this category are

- **Monoidal**: You can concat any two diagrams together to get a larger
diagram. The resulting interface is just the concatenation of the two.

- **Braided**: there is a way to exchange any two halves of the diagram
interface without changing the behavior. This property can be used to
permute an interface in any way.

The two interface lists of each diagram actually contain two kinds of types:

- **Push**: These represent ports for messages coming from the source and
traveling to the destination. Push drivers can send messages at any time.

- **Pull**: These represent views of some source as seen by the destination.
Pull drivers can query views at any time.

Push and pull diagrams can interact through mutable variables, support for
which must be provided before a program will run. Each time a push message
arrives at a mutable variable it updates, and queries of the variable always
correspond to the last message event's value.

Each program effectively runs in its own thread and can be wait-joined by
a monitoring thread or many programs can be spawned together. If any driver
experiences an exception then all resources are released. Auto respawning
of driver workers is not yet supported.

The basic push message transformer is a functor E (for event) which maps
A to [E A] and f :: A -> B to a diagram of type D r [E A] [E B] for some 
resource type r. Similarly the basic view transformer is a functor V which
maps A to [V A] and g :: A -> B to a diagram of type D r [V A] [V B].
Mutable variables are the natural transformation from E to V which maps type
A to a diagram of type D r [E A] [V A]. This diagram has to remember the
last message seen in order to satisfy the natural transformation laws.

## Description of primitives

f a and g a mean that E or V can be used. Other functors will not produce
a runnable program. Some primitives are specific to either E or V. The
DataKinds extension required to write the diagram types requires you to
put a quote to denote the type-level list constructor.

### `ident :: D r '[f a] '[f a]`
The identity diagram is a simple connection and has no effect.

### `dmap :: (a -> b) -> D r '[f a] '[f b]`
Lift a function using either the E or V functor to get a message or
query transformer.

### `(>>>) :: D r i j -> D r j k -> D r i k`
Compose two diagrams that have compatible ports

### `(<>) :: D r i j -> D r i' j' -> D r (i :+: i') (j :+: j')`
Concat two diagrams. The resulting interface is the concatenation.

### `empty :: D r '[] '[]`
The empty diagram has no ports and so does nothing. It's the identity for
diagram concat.

### `swap :: D r '[f a, g b] '[g b, f a]`
Swaps two ports.

### `copy :: D r '[f a] '[f a, f a]`
Copy a message going from source to two destinations, or merge two queries to
use the same source. There is no defined time ordering of copied messages
so don't rely on it.

### `merge :: D r '[E a, E a] '[E a]`
Merge two message channels going right.

### `hole :: D r '[f a] '[]`
Messages entering here will be lost. Useful for ignoring a port. Viewable sources
will never be queried by this.

### `never :: D r '[] '[E a]`
A message source that never sends anything. Useful for ignoring a port.

### `always :: a -> D r '[] '[V a]`
When queried this will always produce the same result.

### `apply :: D r '[V (a -> b), V a] '[V b]`
Merge two queries where one is a function

### `snap :: (a -> b -> c) -> D r '[E a, V b] '[E c]`
On the event do a query and combine with a function.

### `request :: (r -> Resource a b c) -> D r '[E a] '[E b, V c]`
Do an external request to a resource. The response will appear as a new
message. The resource may also be queried. This is useful for implementing
IO based components. The resource must be provided before the program will
run. 

### `var :: (r -> Storage a) -> D r '[E a] '[V a]`
A mutable variable whose storage (and so initial value) must be provided at
launch time. Mutable variables are the natural transformations from the E
functor to the V functor.

### `trace :: D r (f a ': i) (f a ': j) -> D r i j`
Connect the first source and destination port with a loop. Certain bad loops
are not allowed and will be rejected before a program can run. Valid traces have
no effect in diagrams consisting only of push or only of pull connections.

## Drivers

After constructing a diagram you can attach various drivers to communicate
with each other. There are four kinds of drivers that correspond with the
input and output of push and pull diagram interfaces.

When an E type appears on the source, then a worker must be provided who will
take a command and run in the background, executing the command when it wants.

When an E type appears on the destination, then an external command must be
provided to handle the message. For best results this command should not block.

When a V type appears on the source, then a viewable source must be provided
to produce a value on demand. For best results this query should not block.

When a V type appears on the destination, then a worker must be provided who
will take a query and run in the background, executing the query when it wants.

[logo]: https://raw.githubusercontent.com/evanrinehart/braided-monoidal-combinators/master/image.png "Combinator Symbols"
