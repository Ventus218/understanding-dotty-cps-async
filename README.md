# Notes

## Inferring the async argument type

Let's consider this simplified definition of `async`

```scala
inline def async[F[_]: Monad, A](inline a: A): F[A] = ???
```

This definition cause some problems when inferring `A` in the callsite, look at
the following example

Obviously this cannot compile (what Monad will `a` be wrapped in?)

```scala
val a = 0
async:
  a
```

Now we'd expect this to compile:

```scala
val a = 0
async[Option]:
  a
```

But it does not, the reason is that once we specify `Option` as type parameter
we also need to specify `A`, even though it is clear that it should be `Int`

This works, but it's suboptimal since the argument type can be easilly inferred

```scala
val a = 0
async[Option, Int]:
  a
```

The way dotty-cps-async solves this is splitting the two type parameters by
using a class which "stores" the first type parameter(look at `InferAsyncArg`).
What they do is something like this:

```scala
inline def async[F[_]: Monad] = InferAsyncArg.apply

class InferAsyncArg[F[_]: Monad]:
  inline def apply[A](inline a: A): F[A] = ???
```

In scala >= 3.6.2 this could be rewritten like:

```scala
inline def async[F[_]](using Monad[F])[A](inline a: A): F[A] = ???
```
