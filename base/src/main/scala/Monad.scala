package scalaFP

//TODO Should there be a default (Applicative, Bind) => Monad instance?
trait Monad[F[_]] extends Applicative[F] with Bind[F]

object Monad {
  def apply[F[_]](implicit e: IMonad[F]) = e.run

  trait From[F[_]] extends Monad[F] with Applicative.From[F] with Bind.From[F] {
    def monadDelegate: Monad[F]
    def bindDelegate = monadDelegate
    def applicativeDelegate = monadDelegate
    override def applyDelegate = monadDelegate
  }

  trait FromBindPure[F[_]] extends Monad[F] with Bind.FromBindMap[F] { self =>
    override def pure[A](a: A): F[A]
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
    override def map[A, B](fa: F[A])(f: A => B): F[B] = bind(fa)(f.andThen(pure))
  }

  trait FromPureMapJoin[F[_]] extends Monad[F] with Bind.FromJoinMap[F]
}
