# Understanding dotty-cps-async

The aim of this repo is to try to reimplement
[dotty-cps-async](https://github.com/dotty-cps-async/dotty-cps-async#dotty-cps-async)
in order to better understand how it works.

Regarding the theory i referred mostly to
[Project Paper: Embedding generic monadic transformer into Scala](https://ar5iv.labs.arxiv.org/html/2209.10941).

For the implementation part i focused on trying to do it by myself and after
that compare it to what has been done in dotty-cps-async.

## Introduction

What we want to achieve is to be able to turn any monadic program into a
direct-style program.

This should ease the use of monads for anybody used to the imperative
programming paradigm while still keeping most of the benefits of functional
programming.

Monads `flatMap` can be interpreted as a way to chain computations and this
reminds of continuation passing style (CPS) which is a way of expressing
programs in which "the next thing to do" is passed to a function which then
executes it.

Transforming a direct-style program into a CPS program through a monad will
inject the specific monad behaviour inside that program.

As an example:

```scala
def getResource(): Future[Resource] = ???
def updateResource(r: Resource): Future[Resource] = ???

// Monadic program
getResource().flatMap(r => updateResource(r))

// Direct-style program
async[Future]:
  val resource = await(getResource())
  await(updateResource(resource))
```

### `async` and `await` keywords

The `async[F]` keyword is used to mark a block of code that will be executed in
the context of the `F` monad. While the `await` keyword serves the purpose of
explicitating when the specific monad behaviour happens (in case of the `Future`
monad that behaviour is suspension of the control flow without busy waiting).

The words `async` and `await` make sense in the context of `Future`s,
dotty-cps-async uses this words for every monad but also allows for defining
custom custom keyword that will make more sense in other contexts.

### Macros

dotty-cps-async uses macros to achieve this goal. Macros are a metaprogramming
tool, basically a way to inspect part of a program and manipulate it at compile
time in order to generate a different program.

The hard part of using macros is being able to handle every program which can be
expressed in the target language. dotty-cps-async does that great but it makes
it really hard to understand the fundamentals of the approach by inspecting the
code. And this is why in this project we will only support the most essential
and basic language constructs.

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
of code. Blocks are composed of a list of `statements` and a final `expression`.

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
      case (valDef @ ValDef(name, typeTree, Some(rhs))) :: next =>
        typeTree.tpe.asType match
          case '[t] =>
            val rhsExpr = asyncImpl(rhs.asExprOf[t])
            val lambda = Lambda(
              Symbol.spliceOwner,
              MethodType(List(name))(
                _ => List(typeTree.tpe),
                _ => TypeRepr.of[F[A]]
              ),
              (lambdaSymbol, args) =>
                val nextSymbolsChanged =
                  next.map(n =>
                    Utils
                      .refsSymbolSubstitutor(valDef.symbol, args.head.symbol)
                      .transformTree(n)(n.symbol)
                      .asInstanceOf[Statement]
                  )
                val exprSymbolsChanged =
                  Utils
                    .refsSymbolSubstitutor(valDef.symbol, args.head.symbol)
                    .transformTree(expr)(expr.symbol)
                    .asInstanceOf[Term]
                // Automatic inference is not able to get the right type parameters
                loop[F, A](nextSymbolsChanged, exprSymbolsChanged).asTerm
                  .changeOwner(
                    lambdaSymbol
                  )
            ).asExprOf[t => F[A]]
            '{ $m.flatMap($rhsExpr, $lambda) }
      case (head: Term) :: next =>
        head.tpe.asType match
          case '[t] =>
            val termExpr = asyncImpl(head.asExprOf[t])
            val lambda = Lambda(
              Symbol.spliceOwner,
              MethodType(List("_"))(
                _ => List(TypeRepr.of[t]),
                _ => TypeRepr.of[F[A]]
              ),
              (lambdaSymbol, args) =>
                // Automatic inference is not able to get the right type parameters
                loop[F, A](next, expr).asTerm.changeOwner(lambdaSymbol)
            ).asExprOf[t => F[A]]
            '{ $m.flatMap($termExpr, $lambda) }
      case head :: next =>
        val block = Block(List(head), loop(next, expr).asTerm)
        '{ $m.flatMap(${ block.asExprOf[F[A]] }, $m.pure) }
      case Nil =>
        asyncImpl(expr.asExprOf[A])
```

Here is the type hirearchy of a `Statement`:

```
+- Statement -+- Import
              +- Export
              +- Definition --+- ClassDef
              |               +- TypeDef
              |               +- ValOrDefDef -+- DefDef
              |                               +- ValDef
              |
              +- Term
```

To keep it simple we only support `ValDef` and `Term` while all the other
constructs are handled by keeping the first statement as is and introduce a new
nesting level for the following statements (dotty-cps-async uses an optimization
where multiple subsequent statements without `await`s are combined at the same
nesting level):

```scala
object Test:
  val m = summon[Monad[Option]]
  async[Option]:
    def a(v: Int) = v + 1
    def b(v: Int) = v + 2
    a(3)
  // becomes:
  m.flatMap({
    def a(v: Int) = v + 1
    m.flatMap({
      def b(v: Int) = v + 2
      m.flatMap(
        m.flatMap(m.pure(3)),
        v => m.pure(a(v))
      )
    },
    m.pure
    )
  },
  m.pure
  )
```

`ValDef`s are handled by applying the transform to the right-hand side of the
`val` definition and then `flatMap` on that passing the continuation:

```scala
object Test:
  val m = summon[Monad[Option]]
  async[Option]:
    val a = 1
    val b = 2
    a
  // becomes:
  m.flatMap(
    m.pure(1),
    a => {
      m.flatMap(
        m.pure(2),
        b => {
          m.pure(a)
        }
      )
    }
  )
```

We also handle the case in which statements are `Term`s. In the frame of
functional programming `Term`s in statements position are not relevant, but
dotty-cps-async doesn't prohibit having side effects. An example of `Term` in
statement position could be a `println`.

The way we handle `Term`s is in principle identical to the way we handled
`ValDef`s. The implementation is actually simpler because when dealing with
`ValDef`s we needed to handle the newly introduced symbol while in this case
there's no need for that.

```scala
object Test:
  val m = summon[Monad[Option]]
  async[Option]:
    println("hello")
    println("sequential")
    0
  // becomes:
  m.flatMap(
    m.pure(println("hello")),
    _ => {
      m.flatMap(
        m.pure(println("sequential")),
        _ => {
          m.pure(0)
        }
      )
    }
  )
```

> Note that the previous example explains the idea behind this transformation,
> the actual resulting code would be different since applying the cps transform
> to `println` will trigger the
> [function application transform](#function-application-transform)

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

## Handling awaits

The rationale is that since in the cps transform we always "wrap" expressions
inside the `F` monad in order to `flatMap` on them, if we have an expression
`fa` that is already "wrapped" we should just do nothing.

```scala
val m = summon[Monad[Option]]

async[Option]:
  val a = await(Option(4))
  val b = await(Option(5))
  a + b
// becomes:
m.flatMap(
  Option(4),
  a =>
    m.flatMap(
      Option(5),
      // Note that a + b will actually be transformed according to the function application transform
      b => m.pure(a + b)
    )
  )
```

And this is how we can do that:

```scala
// Await.scala
def await[F[_]: Monad, A](fa: F[A]): A = ???

// Async.scala
def asyncImpl[F[_], A](a: Expr[A])
  (using m: Expr[Monad[F]])(using Quotes, Type[A], Type[F]): Expr[F[A]] =
  import quotes.reflect.*
  a.asTerm match
    case Apply(Apply(TypeApply(f, typeArgs), args), contextArgs)
        // So:
        //   typeArgs    -> [F, A]
        //   args        -> fa: F[A]
        //   contextArgs -> given_Monad_F: Monad[F]
        if f.symbol == Symbol.requiredMethod("lib.await") =>
      args.head.asExprOf[F[A]]
  // ...
```

> Notice that `await` is actually an unimplemented function which is used just
> as a marker that is then processed iside the `asyncImpl` macro.

In order to match on our `await` marker we need to understand what is the actual
signature of `await`

```scala
def await[F[_]: Monad, A](fa: F[A]): A
// is just syntactic sugar for
def await[F[_], A](fa: F[A])(using Monad[F]): A
// which is a curried function and that's why we need to match on Apply(Apply(...))
```

## While transform

This transformation implies transforming the while condition and body and then
using a helper to mimic iteration through recursion:

```scala
object WhileTransform:
  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.While
  ): Expr[F[A]] =
    import quotes.reflect.*
    val conditionExpr = asyncImpl(term.cond.asExprOf[Boolean])
    // We can safely assume while body is Unit
    val bodyExpr = asyncImpl(term.body.asExprOf[Unit])
    '{ whileHelper($conditionExpr, $bodyExpr)(using $m) }.asExprOf[F[A]]

  private def whileHelper[F[_]](
      cond: => F[Boolean],
      body: => F[Unit]
  )(using m: Monad[F]): F[Unit] =
    m.flatMap(
      cond,
      evaluatedCond =>
        if (evaluatedCond) then m.flatMap(body, _ => whileHelper(cond, body))
        else m.pure(())
    )
```

The helper is the actual heart of the transformation, notice that `cond` and
`body` are passed by name so that they can be re-evaluated multiple times after
the global state has changed.

Here a nice example of using while within the State monad:

```scala
type IntState[A] = State[Int, A]
println:
  async[IntState] {
    while 10 > await(State.get[Int]) do
      await(State.modify[Int](_ + 1))
  }.run(0)
// The same as
println:
  async[IntState] {
    while 10 > await(State.get[Int]) do
      val s = await(State.get[Int])
      await(State.set(s + 1))
}.run(0)
// Both print (10, ()) where 10 is the current state
```
