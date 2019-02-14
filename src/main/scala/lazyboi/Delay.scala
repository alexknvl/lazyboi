package lazyboi

import cats.Eval

trait Delay[A] {
  def delay(a: Need[A]): A
}
object Delay {
  def apply[A](implicit A: Delay[A]): Delay[A] = A

  implicit def need[A]: Delay[Need[A]] =
    (a: Need[Need[A]]) => a.flatMap(identity)

  // FIXME: this is probably broken.
  implicit def eval[A]: Delay[Eval[A]] =
    (a: Need[Eval[A]]) => Eval.later(a.value).flatMap(identity)
}

