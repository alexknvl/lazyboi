package lazyboi

trait NFData[A] {
  def force(a: A): Need[A]
}
object NFData {
  def apply[A](implicit A: NFData[A]): NFData[A] = A

  implicit def need[A: NFData]: NFData[Need[A]] =
    (a: Need[A]) => a.map(NFData[A].force(_))
}