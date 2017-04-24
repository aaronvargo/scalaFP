package scalaFP

@meta.typeclass
trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  val toApply: Apply[F]
}

object Applicative {
  def fromApPure[F[_]](ap: Apply._Ap[F], pure: _Pure[F]): Applicative[F] = {
    val x = new Functor._Map[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
    }
    Applicative(pure, Apply(ap, Functor.fromMap(x)))
  }

  def fromPureApply[F[_]](pure: _Pure[F], apply: Apply[F]): Applicative[F] =
    Applicative(pure, apply)
}

trait ApplicativeModule {
  implicit class ApplicativeSyntax0[A](self: A) {
    def pure[F[_]](implicit F: Applicative[F]): F[A] = F.pure(self)
  }
}
