package lazyboi

import cats.{Applicative, Eval, Monad, Monoid, MonoidK, Semigroup, Traverse}

sealed trait Maybe[+A] {
  import Maybe._
  def value: Need[Now[A]]
}
object Maybe {
  sealed trait Now[+A] extends Maybe[A]
  final case class Later[+A](value: Need[Now[A]]) extends Maybe[A]
  final case object None extends Now[Nothing] {
    val value: Need[None.type] = Need.now(this)
  }
  final case class Some[A](get: A) extends Now[A] {
    def value: Need[Some[A]] = Need.now(this)
  }

  implicit def monoid[A: Semigroup]: Monoid[Maybe[A]] = new Monoid[Maybe[A]] {
    override def empty: Maybe[A] = None
    override def combine(x: Maybe[A], y: Maybe[A]): Maybe[A] = {
      Later(x.value.flatMap {
        case None => y.value
        case Some(x) => y.value.flatMap {
          case None => None.value
          case Some(y) => Need(Some(Semigroup[A].combine(x, y)))
        }
      })
    }
  }

  implicit def instance: MonadFix[Maybe] = new MonadFix[Maybe] {
    def unSome[A](m: Maybe[A]): Need[A] =
      m.value.map { case Some(x) => x }

    override def pure[A](a: Need[A]): Maybe[A] = Later(a.map(Some.apply))

    override def mfix[A](f: Need[A] => Maybe[A]): Maybe[A] = {
      lazy val a: Maybe[A] = f(unSome(a))
      a
    }

    override def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
      Later(fa.value.map {
        case None => None
        case Some(a) => Some(f(a))
      })

    override def flatMap[A, B](fa: Maybe[A])(f: A => Maybe[B]): Maybe[B] = Later(fa.value.flatMap {
      case None => None.value
      case Some(a) => f(a).value
    })
  }
}
