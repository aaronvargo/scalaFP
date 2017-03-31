package scalaFP

trait MonadReader[S, F[_]] extends Monad[F] {
  def ask: F[S]
  def local[A](fa: F[A])(f: S => S): F[A]
}

object MonadReader {
  def apply[S, F[_]](implicit e: IMonadReader[S, F]) = e.run
}

trait MonadReaderStdInstances {
  implicit def function1MonadReader[S]: MonadReader[S, S => ?] = ???
    // new MonadReader[S, S => ?] {
    //   def ask = identity
    //   def local[A](fa: S => A)(f: S => S) = f.andThen(fa)
    // }
}
