package lazyboi

trait LazyMonad[F[_]] {
  def pure[A](a: Need[A]): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
}
trait MonadFix[F[_]] extends LazyMonad[F] {
  def mfix[A](f: Need[A] => F[A]): F[A]
}