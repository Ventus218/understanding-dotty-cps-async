import lib.*
import scala.concurrent.*
import State.{flatMap as stateFlatMap, *}

given Monad[Option] with
  def pure[A](a: A): Option[A] = Option(a)
  extension [A](fa: Option[A])
    def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

given Monad[List] with
  def pure[A](a: A): List[A] = List(a)
  extension [A](fa: List[A])
    def flatMap[B](f: A => List[B]): List[B] = fa.flatMap(f)

given (using ec: ExecutionContext): Monad[Future] with
  def pure[A](a: A): Future[A] = Future(a)(using ec)
  extension [A](fa: Future[A])
    def flatMap[B](f: A => Future[B]): Future[B] =
      fa.flatMap(f)(using ec)

given [S]: Monad[[A] =>> State[S, A]] with
  def pure[A](a: A): State[S, A] = State.pure(a)
  extension [A](fa: State[S, A])
    def flatMap[B](f: A => State[S, B]): State[S, B] = fa.stateFlatMap(f)

val m = summon[Monad[Option]]

object Trivial extends App:
  val a = 0
  val b = async[Option]:
    a
  val c = async[Option]:
    ""

object Sequential extends App:
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

  val d1 = async[Option]:
    def a(v: Int) = v + 1
    def b(v: Int) = v + 2
    a(3)

  val e1 = async[Option]:
    println("hello")
    println("sequential")
    3

object FunctionApplication extends App:
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

object Condition extends App:
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

object Await extends App:
  val a1 = async[Option]:
    // why Some does not work?
    // It doesn't in dotty-cps-async as well but you can provide a MonadConversion
    // await(Some(3))
    await(Option(3))
  println(a1)
  val a2 = async[Option]:
    val opt = Option(3)
    await(opt)
  println(a2)
  val b1 = async[Option]:
    await(None)
  println(b1)
  val c1 = async[Option]:
    val a = await(Option(4))
    val b = await(Option(5))
    val c = await(Option.empty)
    a + b
  println(c1)
  val c2 = async[Option]:
    val a = await(Option(4))
    // in dotty-cps-async you need to specify the type parameter as well
    val b = await(Option.empty[Int])
    val c = await(Option(5))
    a + b
  println(c2)
  val c3 = async[Option]:
    val a = await(Option(4))
    val b = await(Option(5))
    a + b
  println(c3)

  val d1 = async[List]:
    val a = await(List(1, 2, 3))
    val b = await(List(4, 5, 6))
    a.toString() + " " + b.toString()
  println(d1)
  // The following example does not compile as i don't handle Repeated (varargs)
  // in the macro.
  // I leave this here as an example of how many different language constructs
  // must be handled to achieve 100% coverage.
  // val d2 = async[List]:
  //   val l = List(1, 2, 3)
  //   val a = await(l)
  //   val b = await(List(4, 5, 6))
  //   a.toString() + " " + b.toString()

  // ********** Testing futures chaining **********
  Future({ Thread.sleep(20); println("before") })
  Future(println("after"))
  // prints:
  // after
  // before
  // It is clear that the futures are executed concurrently as they are eager
  println()
  Thread.sleep(50)
  given ExecutionContext = ExecutionContext.Implicits.global
  async[Future]:
    await(Future({ Thread.sleep(20); println("before") }))
    await(Future(println("after")))
    // prints:
    // before
    // after
    // It is clear that the second future is executed after the first one completes

// We use a def since an object would allow accessing at his resources
// (in this case the given instance and the count var only after his init
// has completed.
// That completion is delayed by the Thread.sleep and then the two Future
// computation start simultaneously which is not what we want for this example
@main def whileTest: Unit =

  type IntState[A] = State[Int, A]
  println:
    async[IntState] {
      while 10 > await(State.get[Int]) do await(State.modify[Int](_ + 1))
    }.run(0)
  // The same as
  println:
    async[IntState] {
      while 10 > await(State.get[Int]) do
        val s = await(State.get[Int])
        await(State.set(s + 1))
    }.run(0)

  given ExecutionContext = ExecutionContext.Implicits.global

  var count = 0
  async[Future]:
    while count < 10 do
      await(Future(println(count)))
      await(Future(count += 1))
      await(Future(Thread.sleep(50)))

  Thread.sleep(1000)

  def randomFuture(): Future[Boolean] =
    println("producing random boolean")
    Future(scala.util.Random.nextInt(5) != 0) // P = 4/5
  count = 0
  async[Future]:
    while await(randomFuture()) do
      await(Future(println(count)))
      await(Future(count += 1))
      await(Future(Thread.sleep(50)))

  Thread.sleep(1000)

  async[Future]:
    while false do await(Future(println("never executed")))
