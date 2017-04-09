package scalaFP

trait MonadReader[S, F[_]] extends Monad[F] {
  def ask: F[S]
  def local[A](fa: F[A])(f: S => S): F[A]
}

object MonadReader {
  def apply[S, F[_]](implicit e: IMonadReader[S, F]) = e.run
}
