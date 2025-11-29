import lib.*

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

  val d1 = async[Option]:
    def a(v: Int) = v + 1
    def b(v: Int) = v + 2
    a(3)

  val e1 = async[Option]:
    println("hello")
    println("sequential")
    3

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
