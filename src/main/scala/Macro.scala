import scala.quoted.*

inline def async[F[_]](using Monad[F]) = new InferAsyncArg[F]
// inline def async[F[_]: Monad] = new InferAsyncArg[F] // This does not work, don't know why

class InferAsyncArg[F[_]: Monad]:
  inline def apply[A](inline a: A): F[A] =
    ${ asyncImpl('a)(using '{ summon[Monad[F]] }) }

def asyncImpl[F[_], A](
    a: Expr[A]
)(using Expr[Monad[F]], Quotes, Type[A], Type[F]): Expr[F[A]] =
  // Type[A] needed because of term.asExprOf[A]
  import quotes.reflect.*
  a.asTerm match
    case Inlined(_, _, term) => asyncImpl(term.asExprOf[A])
    // check if Ref is Okay or if only Ident is
    case t: Ref     => TrivialTransform(t)
    case t: Literal => TrivialTransform(t)
    case t: Block   => SequentialTransform(t)
    // some ASTs are wrapped in Typed, here we just unwrap but should check if
    // there are some unexpected consequences
    case Typed(t, _) => asyncImpl(t.asExprOf[A])
    case _ =>
      println(a.asTerm.toString())
      ???

object TrivialTransform:

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Ref | quotes.reflect.Literal
  ): Expr[F[A]] =
    import quotes.reflect.*
    '{ $m.pure(${ term.asExprOf[A] }) }

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
