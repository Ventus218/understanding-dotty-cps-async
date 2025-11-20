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

## Condition transform

This transformation is responsible for the monadification of `if then else`
expressions.

```scala
object ConditionTransform:

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.If
  ): Expr[F[A]] =
    import quotes.reflect.*
    val conditionExpr = asyncImpl(term.cond.asExprOf[Boolean])
    val lambda = Lambda(
      Symbol.spliceOwner,
      MethodType(List("arg"))(
        _ => List(TypeRepr.of[Boolean]),
        _ => TypeRepr.of[F[A]]
      ),
      (lambdaSymbol, args) =>
        If(
          args.head.asInstanceOf[Term],
          asyncImpl(term.thenp.asExprOf[A]).asTerm.changeOwner(lambdaSymbol),
          asyncImpl(term.elsep.asExprOf[A]).asTerm.changeOwner(lambdaSymbol)
        )
    ).asExprOf[Boolean => F[A]]
    '{ $m.flatMap($conditionExpr, $lambda) }

object Test:
  val m = summon[Monad[Option]]
  async[Option]:
    if true then 0
    else 1
  //becomes:
  m.flatMap(m.pure(true), arg =>
    if arg then 0
    else 1
  )
```

## Function application transform

Up to now we've only implemented the function application transform when the
function arguments are non-functional types.

```scala
object FunctionApplicationTransform:
  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Apply
  ): Expr[F[A]] =
    import quotes.reflect.*
    loop(term.fun, term.args)

  def loop[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      f: quotes.reflect.Term,
      args: List[quotes.reflect.Term],
      transformedArgs: List[quotes.reflect.Term] = List()
  ): Expr[F[A]] =
    import quotes.reflect.*
    args match
      case arg :: next =>
        arg.tpe.asType match
          case '[t] =>
            val argExpr = asyncImpl(arg.asExprOf[t])
            val lambda = Lambda(
              Symbol.spliceOwner,
              MethodType(List(arg.symbol.name))(
                _ => List(arg.tpe),
                _ => TypeRepr.of[F[A]]
              ),
              (lambdaSymbol, args) =>
                // We know there will be one arg due to the MethodType.
                // Cast SHOULD be safe as that's what they also do in the doc
                // there is an open issue on that:
                // https://github.com/scala/scala3/issues/23038
                val arg = args.head.asInstanceOf[Term]
                loop[F, A](f, next, transformedArgs :+ arg).asTerm.changeOwner(
                  lambdaSymbol
                )
            ).asExprOf[t => F[A]]
            '{ $m.flatMap($argExpr, $lambda) }
      case Nil => '{ $m.pure(${ Apply(f, transformedArgs).asExprOf[A] }) }
```

To transform a function application in continuation passing style we just need
to transform each argument one by one and for each argument we introduce a
flatMap nesting level.

```scala
  val m = summon[Monad[Option]]

  def myF(a: Int, b: Int, c: Int) = a + b + c

  async[Option]:
    myF(1, 2, 3)
  // becomes:
  m.flatMap(
    m.pure(1),
    a =>
      m.flatMap(
        m.pure(2),
        b =>
          m.flatMap(
            m.pure(3),
            c =>
              m.pure(
                myF(a, b, c)
              )
          )
      )
  )
```
