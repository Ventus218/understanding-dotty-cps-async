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
    // check if Ref is Okay or if only Ident is
    case t: Ref     => TrivialTransform(t)
    case t: Literal => TrivialTransform(t)
    // case Lambda(params, body) => LambdaTransform(params, body)
    case t: Block => SequentialTransform(t)
    case t: If    => ConditionTransform(t)
    case t: Apply => FunctionApplicationTransform(t)
    // Not 100% sure about the correctness of the following transforms
    case Inlined(_, _, term) => asyncImpl(term.asExprOf[A])
    case t: TypeApply        => TrivialTransform(t)
    case t: Typed            => TrivialTransform(t)
    case _ =>
      println(a.asTerm)
      ???

object TrivialTransform:

  def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
      term: quotes.reflect.Ref | quotes.reflect.Literal |
        quotes.reflect.TypeApply | quotes.reflect.Typed
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
      case head :: next =>
        val block = Block(List(head), loop(next, expr).asTerm)
        '{ $m.flatMap(${ block.asExprOf[F[A]] }, $m.pure) }
      case Nil =>
        asyncImpl(expr.asExprOf[A])

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

// object LambdaTransform:
//   // Lambda is convenient for apply and unapply but it actually is a Block with a DefDef and a Closure
//   def apply[F[_]: Type, A: Type](using m: Expr[Monad[F]])(using Quotes)(
//       params: List[quotes.reflect.ValDef],
//       body: quotes.reflect.Term
//   ): Expr[F[A]] =
//     import quotes.reflect.*
//     body.tpe.asType match
//       case '[bodyT] =>
//         Lambda(
//           Symbol.spliceOwner,
//           MethodType(params.map(_.name))(
//             _ => params.map(_.tpt.tpe),
//             _ => TypeRepr.of[F[A]]
//           ),
//           (lambdaSymbol, args) =>
//             body.changeOwner(lambdaSymbol)
//             val newBody = params
//               .zip(args)
//               .foldLeft(body)((body, params) =>
//                 val (old, new_) = params
//                 Utils
//                   .refsSymbolSubstitutor(old.symbol, new_.symbol)
//                   .transformTerm(body)(lambdaSymbol)
//               )
//             asyncImpl(newBody.asExpr).asTerm.changeOwner(lambdaSymbol)
//         ).asExprOf[F[A]]

object Utils:

  def refsSymbolSubstitutor(using Quotes)(
      s: quotes.reflect.Symbol,
      to: quotes.reflect.Symbol
  ): quotes.reflect.TreeMap =
    import quotes.reflect.*
    new TreeMap:
      override def transformTerm(tree: Term)(owner: Symbol): Term =
        tree match
          case ref: Ref if ref.symbol == s =>
            Ref(to)
          case _ =>
            super.transformTerm(tree)(owner)
