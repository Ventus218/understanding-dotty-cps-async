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
    case _ =>
      println(a.asTerm.toString())
      ???

object TrivialTransform:

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Ref | quotes.reflect.Literal
  ): Expr[F[A]] =
    import quotes.reflect.*
    '{ $m.pure(${ term.asExprOf[A] }) }
