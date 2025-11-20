trait Monad[F[_]]:
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A], f: A => F[B]): F[B]
  def map[A, B](fa: F[A], f: A => B): F[B] =
    flatMap(fa, a => pure(f(a)))

given Monad[Option] with
  def pure[A](a: A): Option[A] = Option(a)
  def flatMap[A, B](fa: Option[A], f: A => Option[B]): Option[B] = fa.flatMap(f)

given Monad[List] with
  def pure[A](a: A): List[A] = List(a)
  def flatMap[A, B](fa: List[A], f: A => List[B]): List[B] = fa.flatMap(f)

val m = summon[Monad[Option]]

object Trivial:
  val a = 0
  val b = async[Option]:
    a
  val c = async[Option]:
    ""

object Sequential:
  val a1 = Option:
    val d = 3
  val a2 = async[Option]:
    val d = 3

  val b1 = Option:
    val d = 3
    val e = 4
    val f = e
  val b2 = async[Option]:
    val d = 3
    val e = 4
    val f = e

  val c1 = Option:
    val d = 3
    val e = 4
    val f = e
    f
  val c2 = async[Option]:
    val d = 3
    val e = 4
    val f = e
    f

object FunctionApplication:
  val d1 = async[Option]:
    print("hello")
    val d = 3
    val f = d
    f
  val d2 = async[Option]:
    val d = 3
    print("hello")
    val f = d
    f
  val d3 = async[Option]:
    val d = 3
    val f = d
    println("hello")
  val d4 = async[Option]:
    val d = "3"
    val f = d
    println(d)
  val d5 = async[Option]:
    val d = "3"
    val f = d
    println()

  val e = async[Option]:
    val d = 3
    val f = d
    f.toString()

  val f = async[Option]:
    val a = "a"
    val b = "b"
    a + b

  def myF(d: Int, e: Int, f: Int) = d + e + f
  val g1 = async[Option]:
    val a = 1
    val b = 2
    val c = 3
    myF(a, b, c)
  val g2 = async[Option]:
    myF(1, 2, 3)

// object Lambda:
//   val a = async[Option]:
//     print("hello")
//     (a: Int) => a + 1

object Condition:
  val a1 = Option:
    val cond = true
    if cond then 0
    else 1
  val a2 = async[Option]:
    if true then 0
    else 1
  val a3 = async[Option]:
    val cond = true
    if cond then 0
    else 1

object ExampleOfHowManyEdgeCasesNeedToBeHandled:
  async[Option]:
    val a = List.empty[Int]
    val b = a :+ 1
    val c = b :+ 1
    c
  // async[Option]:
  //   val a = List(1, 2, 3) // Varargs are Repeated terms
  //   a
