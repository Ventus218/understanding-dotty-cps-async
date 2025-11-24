package lib:
  def await[F[_]: Monad, A](fa: F[A]): A = ???
