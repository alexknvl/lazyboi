package lazyboi

trait WHNFData[A] {
  def force(a: A): Need[A]
}
object WHNFData {
  def apply[A](implicit A: WHNFData[A]): WHNFData[A] = A

  implicit def need[A: WHNFData]: WHNFData[Need[A]] =
    (a: Need[A]) => a.map(WHNFData[A].force(_))
}