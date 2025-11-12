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
    case Inlined(_, _, term)     => asyncImpl(term.asExprOf[A])
    case TrivialTransform(ident) => TrivialTransform(ident)

object TrivialTransform:

  def unapply[F[_]](using Quotes)(
      term: quotes.reflect.Term
  ): Option[quotes.reflect.Term] =
    import quotes.reflect.*
    term match
      case t: Ident   => Some(t)
      case t: Literal => Some(t)

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Term
  ): Expr[F[A]] =
    import quotes.reflect.*
    '{ $m.pure(${ term.asExprOf[A] }) }
