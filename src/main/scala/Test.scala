package sos

import lib.Monad
import scala.collection.mutable.Map as MMap
import java.util.UUID
import scala.annotation.threadUnsafe

case class Ref[A](id: String):
  def value[F[_]: Monad](using Context[F, A]): A =
    summon[Context[F, A]].refs(this).get.asInstanceOf[A]

object Ref:
  var count = -1
  // def newRef[A]: Ref[A] = Ref(UUID.randomUUID.toString())
  def newRef[A]: Ref[A] = { count += 1; Ref(count.toString()) }

// given [F[_]: Monad, A](using Context[F, A]): Conversion[Ref[A], A] with
//   def apply(x: Ref[A]): A = x.value

class Context[F[_]: Monad, A]:
  var refs = MMap[Ref[?], Option[?]]()
  var last: Option[(F[A], Ref[?])] = None

def async[F[_], A](block: Context[F, A] ?=> Ref[A])(using m: Monad[F]): F[A] =
  val c = new Context[F, A]()
  val finalRef = block(using c)
  c.last match
    case Some((fa, ref)) =>
      m.map(fa)(res =>
        c.refs(ref) = Some(res)
        c.refs(finalRef).get.asInstanceOf[A]
      )
    case None => m.pure(()).asInstanceOf[F[A]] // TODO: check if always correct

def sync[F[_], A](a: => A)(using Context[F, A])(using m: Monad[F]): Ref[A] =
  await(m.pure(a))

def await[F[_], A](a: => F[A])(using c: Context[F, A], m: Monad[F]): Ref[A] =
  c.last match
    case None =>
      val ref = Ref.newRef[A]
      c.last = Some((a, ref))
      ref
    case Some((fa, ref)) =>
      val newRef = Ref.newRef[A]
      c.last = Some(
        (
          m.flatMap(fa)(res =>
            c.refs(ref) = Some(res)
            a
          ),
          newRef
        )
      )
      newRef

def loop[F[_], A](
    cond: => Boolean
)(body: => Unit)(using c: Context[F, A], m: Monad[F]): Unit =
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
            if cond then body.asInstanceOf[A] // TODO: check
            else ().asInstanceOf[A] // TODO: check
          ),
          newRef
        )
      )

def assign[F[_], A](assignedRef: Ref[A], a: => A)(using
    c: Context[F, A],
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
      sync(a.value + b.value)
    .get == 5

  assert:
    async[Option, Int]:
      loop(false):
        println("loop")
      sync(5)
    .get == 5

  // assert:
  //   async[Option, Int]:
  //     val a = sync(false)
  //     loop(a.value):
  //       println("loop")
  //     sync(5)
  //   .get == 5

  // assert:
  //   async[Option, Int]:
  //     val a = sync(3)
  //     loop(a.value < 5):
  //       println("loop")
  //       assign(a, a.value + 1)
  //     sync(a.value)
  //   .get == 5
