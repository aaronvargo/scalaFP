package scalaFP

@meta.typeclass
trait Apply[F[_]] {
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  val toFunctor: Functor[F]
}

object Apply {
  def fromApMap[F[_]](ap: _Ap[F], map: Functor._Map[F]) = Apply(ap, Functor.fromMap(map))
}

trait ApplyModule {
  implicit class ApplySyntax[F[_], A](self: F[A])(implicit F: Apply[F]) {
    def ap[B](f: F[A => B]): F[B] = F.ap(self)(f)
  }
}
