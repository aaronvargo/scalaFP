package scalaFP

trait Applicative[F[_]] extends Apply[F] with Applicative.Pure[F]

object Applicative {
  def apply[F[_]](implicit e: IApplicative[F]) = e.run

  trait Pure[F[_]] {
    def pure[A](a: A): F[A]
  }

  trait From[F[_]] extends Applicative[F] with Apply.From[F] {
    def applicativeDelegate: Applicative[F]
    def applyDelegate = applicativeDelegate
    def pure[A](a: A): F[A] = applicativeDelegate.pure(a)
  }

  trait FromApPure[F[_]] extends Applicative[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
  }
}
