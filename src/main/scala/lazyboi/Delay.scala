package lazyboi

trait Delay[A] {
  def delay(a: Need[A]): A
}
object Delay {
  def apply[A](implicit A: Delay[A]): Delay[A] = A

  implicit def need[A]: Delay[Need[A]] =
    (a: Need[Need[A]]) => a.flatMap(identity)
}

