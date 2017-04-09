package scalaFP

trait Monad[F[_]] extends Applicative[F] with Bind[F] {
  override def pure[A](a: A): F[A]
  override def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = bind(fa)(f.andThen(pure))
}

object Monad {
  def apply[F[_]](implicit e: IMonad[F]) = e.run

  trait From[F[_]] extends Monad[F] with Applicative.From[F] with Bind.From[F] {
    def monadDelegate: Monad[F]
    override def bindDelegate: Bind[F] = monadDelegate
    override def applicativeDelegate: Applicative[F] = monadDelegate
  }

  trait FromJoin[F[_]] extends Monad[F] with Bind.FromJoin[F] {
    override def pure[A](a: A): F[A]
    override def map[A, B](fa: F[A])(f: A => B): F[B]
    override def join[A, B](fa: F[F[A]]): F[A]
  }
}
