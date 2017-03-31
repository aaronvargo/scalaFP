package scalaFP

trait MonadReader[S, F[_]] extends Monad[F] {
  def ask: F[S]
  def local[A](fa: F[A])(f: S => S): F[A]
}

object MonadReader {
  def apply[S, F[_]](implicit e: IMonadReader[S, F]) = e.run
}

trait MonadReaderModule {
  implicit def function1MonadReader[S]: IMonadReader[S, S => ?] =
    Inst(new MonadReader[S, S => ?] with Monad.FromBindPure[S => ?] {
      def bind[A, B](fa: S => A)(f: A => (S => B)): S => B = s => f(fa(s))(s)
      def pure[A](a: A): S => A = _ => a
      def ask = identity
      def local[A](fa: S => A)(f: S => S) = f.andThen(fa)
    })
}
