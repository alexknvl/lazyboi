package lazyboi

import cats.{Applicative, Eval, Monad, Monoid, MonoidK, Semigroup, Show, Traverse}
import cats.syntax.semigroup._
import cats.syntax.show._

import scala.annotation.tailrec
import scala.collection.mutable

// A dynamization scheme for monoids based on a modified zeroless binary number system
//
// Based on an ekmett talk on cache oblivious data structures
sealed trait MZBDynamizer[+A] {
   import MZBDynamizer._

//
//   def foldLeft[Z](z: Z)(f: (Z, A) => Z): Z = {
//     @tailrec def go(list: LazyList[A], z: Z): Z =
//       list.force match {
//         case Nil => z
//         case Cons(a, as) => go(as, f(z, a))
//       }
//     go(this, z)
//   }

//   def foldRight[Z: Delay](z: Z)(f: (A, Z) => Z): Z = Delay[Z].delay(value.map {
//     case Nil => z
//     case Cons(a, as) => f(a, as.foldRight(z)(f))
//   })

//   def toList: List[A] = {
//     @tailrec def go(list: LazyList[A], builder: mutable.Builder[A, List[A]]): List[A] =
//       list.force match {
//         case Nil => builder.result()
//         case Cons(a, as) => go(as, builder += a)
//       }
//     go(this, List.newBuilder[A])
//   }

//   def size: Int = {
//     @tailrec def go(list: LazyList[A], count: Int): Int =
//       list.force match {
//         case Nil => count
//         case Cons(_, as) => go(as, count + 1)
//       }
//     go(this, 0)
//   }

//   def reverse: LazyList[A] = {
//     @tailrec def go(list: LazyList[A], result: Strict[A]): Strict[A] =
//       list.force match {
//         case Nil => result
//         case Cons(a, as) => go(as, Cons(a, result))
//       }
//     go(this, Nil)
//   }

//   // foldRight(that)(Cons.apply)
//   def ++[AA >: A](that: LazyList[AA]): LazyList[AA] =
//     Lazy(value.flatMap {
//       case Nil => that.value
//       case Cons(a, as) => Need(Cons(a, as ++ that))
//     })

//   def map[B](f: A => B): LazyList[B] =
//     Lazy(value.flatMap {
//       case Nil => Need.now(Nil)
//       case Cons(x, xs) => Need(Cons(f(x), xs.map(f)))
//     })

//   def filter(f: A => Boolean): LazyList[A] =
//     Lazy(value.flatMap {
//       case Nil => Need.now(Nil)
//       case Cons(a, as) =>
//         if (f(a)) Need.now(Cons(a, as.filter(f)))
//         else as.filter(f).value
//     })

//   def toStream: Stream[A] = this.force match {
//     case Nil => Stream.empty
//     case Cons(a, as) => Stream.cons(a, as.toStream)
//   }

//   def distinct: LazyList[A] = {
//     def go(list: LazyList[A], seen: Set[A]): LazyList[A] =
//       Lazy(list.value.flatMap {
//         case Nil => Need.now(Nil)
//         case Cons(a, as) =>
//           if (!seen(a)) Need.now(Cons(a, go(as, seen + a)))
//           else go(as, seen + a).value
//       })

//     go(this, Set.empty)
//   }

//   def withFilter(f: A => Boolean): LazyList[A] = filter(f)

//   def flatMap[B](f: A => LazyList[B]): LazyList[B] =
//     Lazy(value.flatMap {
//       case Nil => Need.now(Nil)
//       case Cons(x, xs) => (f(x) ++ xs.flatMap(f)).value
//     })
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

  def value[A](dyn: MZBDynamizer[A])(implicit A: Monoid[A]): Need[A] = dyn match {
    case D0 => Need.now(A.empty)
    case D1(fst) => Need.now(fst)
    case D2(_, _, fstsnd, tail) => for { fse <- fstsnd; t <- value(tail) } yield fse |+| t
    case D3(fst, _, _, sndthd, tail) => for { ste <- sndthd; t <- value(tail) } yield fst |+| ste |+| t
  }

 // `f` should be a semigroup homomorphism
  def query[A, B: Monoid](dyn: MZBDynamizer[A])(f: A => B): B = dyn match {
    case D0 => Monoid[B].empty
    case D1(fst) => f(fst)
    case D2(fst, snd, _, tail) => f(fst) |+| f(snd) |+| query(tail)(f)
    case D3(fst, snd, thd, _, tail) => f(fst) |+| f(snd) |+| f(thd) |+| query(tail)(f)
  }

 // def +:[AA >: A](newHead: AA)(implicit AA: Semigroup[AA]): MZBDynamizer[AA] = cons(newHead)


  // implicit def traverse: Traverse[MZBDynamizer] =
    // new Traverse[MZBDynamizer] {
      // // for this to have a useful result,
      // // the function argument to `traverse` must be a valid
      // // monoid homomorphism in the natural way.
      // override def traverse[G[_], A, B](fa: MZBDynamizer[A])(f: A => G[B])(implicit ev: Applicative[G]): G[MZBDynamizer[B]] =
        // fa.value.value match {
          // case Nil => ev.pure(Nil)
          // case Cons(a, as) => ev.map2(f(a), traverse(as)(f))(Cons.apply)
        // }

      // override def foldLeft[A, B](fa: D1[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

      // override def foldRight[A, B](fa: D1[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
        // fa.foldRight(lb)(f)

    // }

  // implicit def show[A: Show]: Show[MZBDynamizer[A]] =
    // (t: LazyList[A]) => t.toList.map(_.show).mkString("LazyList(", ", ", ")")
}

