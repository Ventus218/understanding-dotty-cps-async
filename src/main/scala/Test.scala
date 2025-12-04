package sos

import lib.Monad
import scala.collection.mutable.Map as MMap
import java.util.UUID
import scala.concurrent.*
import scala.util.Try

case class Ref[A](id: String):
  def value[F[_]](using Context[F]): A =
    summon[Context[F]].refs(this).get.asInstanceOf[A]

object Ref:
  var count = -1
  // def newRef[A]: Ref[A] = Ref(UUID.randomUUID.toString())
  def newRef[A]: Ref[A] = { count += 1; Ref(count.toString()) }

given [F[_], A](using Context[F]): Conversion[Ref[A], A] with
  def apply(x: Ref[A]): A = x.value

class Context[F[_]: Monad]:
  var refs = MMap[Ref[?], Option[?]]()
  var last: Option[(F[Any], Ref[?])] = None

def async[F[_], A](block: Context[F] ?=> Ref[A])(using m: Monad[F]): F[A] =
  val c = new Context[F]()
  val finalRef = block(using c)
  c.last match
    case Some((fa, ref)) =>
      m.map(fa)(res =>
        c.refs(ref) = Some(res)
        c.refs(finalRef).get.asInstanceOf[A]
      )
    case None => m.pure(()).asInstanceOf[F[A]] // TODO: check if always correct

def sync[F[_], A](a: => A)(using Context[F])(using m: Monad[F]): Ref[A] =
  await(m.pure(a))

def await[F[_], A](a: => F[A])(using c: Context[F], m: Monad[F]): Ref[A] =
  c.last match
    case None =>
      val ref = Ref.newRef[A]
      c.last = Some((a.asInstanceOf[F[Any]], ref))
      ref
    case Some((fa, ref)) =>
      val newRef = Ref.newRef[A]
      c.last = Some(
        (
          m.flatMap(fa)(res =>
            c.refs(ref) = Some(res)
            a.asInstanceOf[F[Any]]
          ),
          newRef
        )
      )
      newRef

def loop[F[_], A](cond: => Boolean)(
    body: => Unit
)(using c: Context[F], m: Monad[F]): Unit =
  c.last match
    case None =>
      if cond then
        body
        loop(cond)(body)
    case Some((fa, ref)) =>
      val newRef = Ref.newRef[Unit] // Even if useless
      c.last = Some(
        (
          m.map(fa)(res =>
            c.refs(ref) = Some(res)
            if cond then
              body
              loop(cond)(body)
            else ()
          ),
          newRef
        )
      )

def assign[F[_], A](assignedRef: Ref[A], a: => A)(using
    c: Context[F],
    m: Monad[F]
): Unit =
  c.last match
    case None =>
      c.last = Some((m.pure(a), assignedRef))
    case Some((fa, ref)) =>
      c.last = Some(
        (
          m.map(fa)(res =>
            c.refs(ref) = Some(res)
            a
          ),
          assignedRef
        )
      )

given Monad[Option] with
  def pure[A](a: A): Option[A] = Option(a)
  extension [A](fa: Option[A])
    def flatMap[B](f: A => Option[B]): Option[B] = fa.flatMap(f)

given Monad[List] with
  def pure[A](a: A): List[A] = List(a)
  extension [A](fa: List[A])
    def flatMap[B](f: A => List[B]): List[B] = fa.flatMap(f)

given Monad[LazyList] with
  def pure[A](a: A): LazyList[A] = LazyList(a)
  extension [A](fa: LazyList[A])
    def flatMap[B](f: A => LazyList[B]): LazyList[B] = fa.flatMap(f)

given (using ec: ExecutionContext): Monad[Future] with
  def pure[A](a: A): Future[A] = Future(a)(using ec)
  extension [A](fa: Future[A])
    def flatMap[B](f: A => Future[B]): Future[B] =
      fa.flatMap(f)(using ec)

object Test extends App:
  assert:
    async[Option, Int]:
      val a = sync(3)
      a
    .get == 3

  assert:
    async[Option, Int]:
      val a = await(Option(3))
      a
    .get == 3

  assert:
    async[Option, Int]:
      val a = await(Option(3))
      val b = await(Option(2))
      b
    .get == 2

  assert:
    async[Option, Int]:
      val a = await(Option.empty[Int])
      val b = await(Option(2))
      b
    .isEmpty

  assert:
    async[Option, Int]:
      val a = await(Option(2))
      val b = await(Option.empty[Int])
      b
    .isEmpty

  assert:
    async[Option, Int]:
      val a = sync(3)
      val b = await(Option.empty[Int])
      a
    .isEmpty

  assert:
    async[Option, Int]:
      val a = sync(3)
      val b = await(Option(2))
      a
    .get == 3

  assert:
    async[Option, Int]:
      val a = sync(3)
      val b = await(Option(2))
      sync(a + b)
    .get == 5

  // // THIS ONE FAILS: List(4, 6, 4, 6)
  // assert:
  //   (async[List, Int]:
  //     val a = await(List(1, 2))
  //     val b = await(List(2, 4))
  //     sync(a + b)
  //   ) == List(2, 2, 4, 4)

  assert:
    async[LazyList, Int]:
      val a = await(LazyList(1, 2))
      val b = await(LazyList(2, 4))
      sync(a + b)
    .force == List(3, 5, 4, 6)

  assert:
    async[Option, Int]:
      loop(false):
        sync(println("loop"))
      sync(5)
    .get == 5

  assert:
    async[Option, Int]:
      val a = sync(3)
      loop(a < 7):
        assign(a, a + 1)
      a
    .get == 7

  given ExecutionContext = ExecutionContext.Implicits.global
  async[Future, Unit]:
    sync(println("0"))
    await(Future(Thread.sleep(300)))
    sync(println("1"))
    await(Future(Thread.sleep(300)))
    sync(println("2"))
    sync(println())

  Thread.sleep(1000)

  async[Future, Int]:
    sync(println("0"))
    val a = await(Future({ Thread.sleep(300); 3 }))
    sync(println("1"))
    await(Future(Thread.sleep(300)))
    sync(println("2"))
    sync(println())
    a
  .onComplete(res => assert(res == Try(3)))

  Thread.sleep(1000)

  val f = Future(Thread.sleep(1))
  async[Option, Int]:
    loop(!f.isCompleted):
      sync(println("loop"))
    sync(3)
