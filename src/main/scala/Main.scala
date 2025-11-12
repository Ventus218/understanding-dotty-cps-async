trait Monad[F[_]]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  def map[A, B](fa: F[A], f: A => B): F[B] =
    flatMap(fa, a => pure(f(a)))

given Monad[Option]:
  def pure[A](a: A): Option[A] = Option(a)
  def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)

given Monad[List]:
  def pure[A](a: A): List[A] = List(a)
  def flatMap[A, B](fa: List[A], f: A => List[B]): List[B] = fa.flatMap(f)

object Trivial:
  val a = 0
  val res = async[Option]:
    a
