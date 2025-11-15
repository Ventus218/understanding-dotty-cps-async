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

## Trivial transform

This transformation is responsible for the monadification of constants and
identifiers, this is done by just wrapping them in the monad using `pure`

```scala
object TrivialTransform:
  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Ref | quotes.reflect.Literal
  ): Expr[F[A]] =
    import quotes.reflect.*
    '{ $m.pure(${ term.asExprOf[A] }) }
```

## Sequential transform

This transformation is responsible for the monadification of sequential blocks
of code.

The simplest way to do so is by transforming the block in CPS where each
statement introduce a new nesting level. (dotty-cps-async uses an optimization
where multiple subsequent statements without `await`s are combined at the same
nesting level)

```scala
object SequentialTransform:

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Block
  ): Expr[F[A]] =
    import quotes.reflect.*
    loop(term.statements, term.expr)

  private def loop[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      statements: List[quotes.reflect.Statement],
      expr: quotes.reflect.Term
  ): Expr[F[A]] =
    import quotes.reflect.*
    statements match
      case head :: next =>
        val block = Block(List(head), loop(next, expr).asTerm)
        // TODO: Mapping should be okay until we introduce await
        '{ $m.map(${ block.asExprOf[F[A]] }, identity) }
      case Nil => asyncImpl(expr.asExprOf[A])

object Test:
  val m = summon[Monad[Option]]
  async[Option]:
    val d = 3
    val e = 4
    val f = e
    f
  // becomes:
  m.map({
      val d = 3
      m.map({
          val e = 4
          m.map({
              val f = e
              m.pure(f)
          },
          identity
          )
      },
      identity
      )
  },
  identity
  )
```

Right now it is enough to use map since we haven't introduced `await`s yet
