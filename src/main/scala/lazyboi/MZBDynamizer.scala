package lazyboi

import cats.{Applicative, Eval, Foldable, Monad, Monoid, MonoidK, Semigroup, Show}
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable

// A dynamization scheme for monoids based on a modified zeroless binary number system
//
// Based on an ekmett talk on cache oblivious data structures
sealed trait MZBDynamizer[+A] {
   import MZBDynamizer._

   def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z = {
     @tailrec def go(dyn: MZBDynamizer[A], z: Z): Z =
       dyn match {
         case D0 => z
         case D1(a) => f(z, a)
         case D2(a, b, _, t) => go(t, f(f(z, a), b))
         case D3(a, b, c, _, t) => go(t, f(f(f(z, a), b), c))
       }
     go(this, z)
   }

  def foldRight[Z](z: Z)(f: (A, Z) => Z)
                   (implicit Z: Delay[Z]): Z = this match {
    case D0 =>
      z
    case D1(a) =>
      f(a, z)
    case D2(a, b, _, t) =>
      f(a, Z.delay(Need(f(b, t.foldRight(z)(f)))))
    case D3(a, b, c, _, t) =>
      f(a, Z.delay(Need(f(b, Z.delay(Need(f(c, t.foldRight(z)(f))))))))
   }

  def toList: List[A] = {
    @tailrec def go(dyn: MZBDynamizer[A], builder: mutable.Builder[A, List[A]]): List[A] =
      dyn match {
        case D0 =>
          builder.result()
        case D1(a) =>
          (builder += a).result()
        case D2(a, b, _, t) =>
          go(t, builder += a += b)
        case D3(a, b, c, _, t) =>
          go(t, builder += a += b += c)
      }
    go(this, List.newBuilder[A])
  }

   def size: Int = {
     @tailrec def go(dyn: MZBDynamizer[A], count: Int): Int =
       dyn match {
         case D0 => count
         case D1(_) => count + 1
         case D2(_, _, _, t) => go(t, count + 1)
         case D3(_, _, _, _, t) => go(t, count + 1)
       }
     go(this, 0)
   }

  def map[B](f: A => B): MZBDynamizer[B] =
    this match {
      case D0 => D0
      case D1(a) => D1(f(a))
      case D2(a, b, ab, t) => D2(f(a), f(b), ab.map(f), t.map(f))
      case D3(a, b, c, bc, t) => D3(f(a), f(b), f(c), bc.map(f), t.map(f))
    }

  // Use this if you want to create new intermediate thunks
  // instead of mapping over the existing ones.
  def mapm[B: Monoid](f: A => B): MZBDynamizer[B] =
    this match {
      case D0 => D0
      case D1(a) => D1(f(a))
      case D2(a, b, _, t) =>
        val (fa, fb) = (f(a), f(b))
        D2(fa, fb, Need(fa |+| fb), t.mapm(f))
      case D3(a, b, c, _, t) =>
        val (fa, fb, fc) = (f(a), f(b), f(c))
        D3(fa, fb, fc, Need(fa |+| fb |+| fc), t.mapm(f))
    }

  // for this to have a useful result,
  // the function argument to `traverse` must be a valid
  // monoid homomorphism in the natural way.
  def traversem[G[_], B: Monoid](f: A => G[B])(implicit G: Applicative[G]): G[MZBDynamizer[B]] =
    this match {
      case D0 => G.pure(D0)
      case D1(a) => G.map(f(a))(D1(_))
      case D2(a, b, _, t) =>
        (f(a), f(b), t.traversem(f)).mapN {
          (fa, fb, te) => D2(fa, fb, Need(fa |+| fb), te)
        }
      case D3(a, b, c, _, t) =>
        (f(a), f(b), f(c), t.traversem(f)).mapN {
          (fa, fb, fc, te) => D3(fa, fb, fc, Need(fb |+| fc), te)
        }
    }
}

object MZBDynamizer {
  def apply[A](as: A*)(implicit AA: Semigroup[A]): MZBDynamizer[A] =
    as.foldLeft(D0: MZBDynamizer[A])(cons(_)(_))

  def empty[A]: MZBDynamizer[A] = D0

  final case object D0 extends MZBDynamizer[Nothing]

  final case class D1[+A](fst: A) extends MZBDynamizer[A]

  final case class D2[+A](fst: A, snd: A, fstsnd: Need[A], tail: MZBDynamizer[A])
    extends MZBDynamizer[A]

  final case class D3[+A](fst: A, snd: A, thd: A, sndthd: Need[A], tail: MZBDynamizer[A])
    extends MZBDynamizer[A]

  def cons[A](dyn: MZBDynamizer[A])(newHead: A)(implicit A: Semigroup[A]): MZBDynamizer[A] = dyn match {
    case D0 =>
      D1(newHead)
    case D1(fst) =>
      D2(newHead, fst, Need(A.combine(newHead, fst)), D0)
    case D2(fst, snd, fstsnd, tail) =>
      D3(newHead, fst, snd, fstsnd, tail)
    case D3(fst, _, _, sndthd, tail) =>
      D2(newHead, fst, Need(A.combine(newHead, fst)), cons(tail)(sndthd.value))
  }

  def valued[A](dyn: MZBDynamizer[A])(implicit A: Monoid[A], D: Delay[A]): A = dyn match {
    case D0 => A.empty
    case D1(fst) => fst
    case D2(_, _, fstsnd, tail) => D.delay(fstsnd) |+| D.delay(Need(valued(tail)))
    case D3(fst, _, _, sndthd, tail) => fst |+| D.delay(sndthd) |+| D.delay(Need(valued(tail)))
  }

  def value[A](dyn: MZBDynamizer[A])(implicit A: Monoid[A]): A = dyn match {
    case D0 => A.empty
    case D1(fst) => fst
    case D2(_, _, fstsnd, tail) => fstsnd.value |+| value(tail)
    case D3(fst, _, _, sndthd, tail) => fst |+| sndthd.value |+| value(tail)
  }

 // `f` should be a semigroup homomorphism
  def query[A, B: Monoid](dyn: MZBDynamizer[A])(f: A => B): B = dyn match {
    case D0 => Monoid[B].empty
    case D1(fst) => f(fst)
    case D2(fst, snd, _, tail) => f(fst) |+| f(snd) |+| query(tail)(f)
    case D3(fst, snd, thd, _, tail) => f(fst) |+| f(snd) |+| f(thd) |+| query(tail)(f)
  }

 // `f` should be a semigroup homomorphism
  def queryd[A, B: Monoid](dyn: MZBDynamizer[A])(f: A => B)(implicit D: Delay[B]): B = dyn match {
    case D0 => Monoid[B].empty
    case D1(fst) => f(fst)
    case D2(fst, snd, _, tail) =>
      D.delay(Need(f(fst))) |+|
        D.delay(Need(f(snd))) |+|
          D.delay(Need(queryd(tail)(f)))
    case D3(fst, snd, thd, _, tail) =>
      D.delay(Need(f(fst))) |+|
        D.delay(Need(f(snd))) |+|
          D.delay(Need(f(thd))) |+|
            D.delay(Need(queryd(tail)(f)))
  }

  implicit val foldable: Foldable[MZBDynamizer] =
    new Foldable[MZBDynamizer] {
      override def foldLeft[A, B](fa: MZBDynamizer[A], b: B)(f: (B, A) => B): B =
        fa.foldLeft(b)(f)
      override def foldRight[A, B](fa: MZBDynamizer[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        fa.foldRight(lb)(f)
    }

  implicit def show[A: Show]: Show[MZBDynamizer[A]] =
    (t: MZBDynamizer[A]) =>
      t.toList.map(_.show).mkString("MZBDynamizer(", ", ", ")")
}

