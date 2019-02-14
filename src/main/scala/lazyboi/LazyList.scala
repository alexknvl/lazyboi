package lazyboi

import cats.{Applicative, Eval, Monad, Monoid, MonoidK, Show, Traverse}
import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable

sealed trait LazyList[+A] {
  import LazyList._

  def value: Need[Strict[A]]

  def force: Strict[A] = this match {
    case s: Strict[A] => s
    case Lazy(value)  => value.value
  }

  def +:[AA >: A](value: AA): Strict[AA] =
    LazyList.Cons(value, this)

  def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z = {
    @tailrec def go(list: LazyList[A], z: Z): Z =
      list.force match {
        case Nil => z
        case Cons(a, as) => go(as, f(z, a))
      }
    go(this, z)
  }

  def foldRight[Z: Delay](z: Z)(f: (A, Z) => Z): Z = Delay[Z].delay(value.map {
    case Nil => z
    case Cons(a, as) => f(a, as.foldRight(z)(f))
  })

  def toList: List[A] = {
    @tailrec def go(list: LazyList[A], builder: mutable.Builder[A, List[A]]): List[A] =
      list.force match {
        case Nil => builder.result()
        case Cons(a, as) => go(as, builder += a)
      }
    go(this, List.newBuilder[A])
  }

  def size: Int = {
    @tailrec def go(list: LazyList[A], count: Int): Int =
      list.force match {
        case Nil => count
        case Cons(_, as) => go(as, count + 1)
      }
    go(this, 0)
  }

  def reverse: LazyList[A] = {
    @tailrec def go(list: LazyList[A], result: Strict[A]): Strict[A] =
      list.force match {
        case Nil => result
        case Cons(a, as) => go(as, Cons(a, result))
      }
    go(this, Nil)
  }

  // foldRight(that)(Cons.apply)
  def ++[AA >: A](that: LazyList[AA]): LazyList[AA] =
    Lazy(value.flatMap {
      case Nil => that.value
      case Cons(a, as) => Need(Cons(a, as ++ that))
    })

  def map[B](f: A => B): LazyList[B] =
    Lazy(value.flatMap {
      case Nil => Need.now(Nil)
      case Cons(x, xs) => Need(Cons(f(x), xs.map(f)))
    })

  def filter(f: A => Boolean): LazyList[A] =
    Lazy(value.flatMap {
      case Nil => Need.now(Nil)
      case Cons(a, as) =>
        if (f(a)) Need.now(Cons(a, as.filter(f)))
        else as.filter(f).value
    })

  def toStream: Stream[A] = this.force match {
    case Nil => Stream.empty
    case Cons(a, as) => Stream.cons(a, as.toStream)
  }

  def distinct: LazyList[A] = {
    def go(list: LazyList[A], seen: Set[A]): LazyList[A] =
      Lazy(list.value.flatMap {
        case Nil => Need.now(Nil)
        case Cons(a, as) =>
          if (!seen(a)) Need.now(Cons(a, go(as, seen + a)))
          else go(as, seen + a).value
      })

    go(this, Set.empty)
  }

  def withFilter(f: A => Boolean): LazyList[A] = filter(f)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] =
    Lazy(value.flatMap {
      case Nil => Need.now(Nil)
      case Cons(x, xs) => (f(x) ++ xs.flatMap(f)).value
    })
}
object LazyList {
  def apply[A](as: A*): LazyList.Strict[A] =
    as.foldRight(Nil: LazyList.Strict[A])(Cons.apply)

  def empty[A]: LazyList[A] = Nil

  def unfold[S, A](s: S)(f: S => Option[(A, S)]): LazyList[A] =
    Lazy(Need {
      f(s) match {
        case None => Nil
        case Some((a, n)) => Cons(a, unfold(n)(f))
      }
    })

  def range(start: Int, end: Int): LazyList[Int] =
    Lazy[Int](Need(
      if (start < end) Cons(start, range(start + 1, end))
      else Nil
    ))

  implicit def delay[A]: Delay[LazyList[A]] =
    (a: Need[LazyList[A]]) => Lazy(a.flatMap(_.value))

  final case class Lazy[A](value: Need[Strict[A]]) extends LazyList[A]
  sealed trait Strict[+A] extends LazyList[A]
  final case object Nil extends Strict[Nothing] {
    val value: Need[Nil.type] = Need.now(this)
  }
  final case class Cons[A](head: A, tail: LazyList[A]) extends Strict[A] {
    val value: Need[Cons[A]] = Need.now(this)
  }

  implicit def monoid[A]: Monoid[LazyList[A]] = new Monoid[LazyList[A]] {
    override def empty: LazyList[A] = Nil
    override def combine(x: LazyList[A], y: LazyList[A]): LazyList[A] = x ++ y
  }
  implicit def monad: Monad[LazyList] with Traverse[LazyList] with MonoidK[LazyList] =
    new Monad[LazyList] with Traverse[LazyList] with MonoidK[LazyList] {
      override def traverse[G[_], A, B](fa: LazyList[A])(f: A => G[B])(implicit ev: Applicative[G]): G[LazyList[B]] =
        fa.value.value match {
          case Nil => ev.pure(Nil)
          case Cons(a, as) => ev.map2(f(a), traverse(as)(f))(Cons.apply)
        }

      override def empty[A]: LazyList[A] = Nil

      override def pure[A](x: A): LazyList[A] = Cons(x, Nil)

      override def foldLeft[A, B](fa: LazyList[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      override def foldRight[A, B](fa: LazyList[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)

      override def flatMap[A, B](fa: LazyList[A])(f: A => LazyList[B]): LazyList[B] = fa.flatMap(f)

      override def tailRecM[A, B](a: A)(f: A => LazyList[Either[A, B]]): LazyList[B] = f(a).flatMap {
        case Left(a1) => tailRecM(a1)(f)
        case Right(b) => Cons(b, Nil)
      }

      override def combineK[A](x: LazyList[A], y: LazyList[A]): LazyList[A] = x ++ y
    }

  implicit def show[A: Show]: Show[LazyList[A]] =
    (t: LazyList[A]) => t.toList.map(_.show).mkString("LazyList(", ", ", ")")
}

