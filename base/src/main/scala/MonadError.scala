package scalaFP

trait MonadError[S, F[_]] extends Monad[F] {
  def raiseError[A](e: S): F[A]
  def handleError[A](fa: F[A])(f: S => F[A]): F[A]
}

object MonadError {
  def apply[S, F[_]](implicit e: IMonadError[S, F]) = e.run
}
