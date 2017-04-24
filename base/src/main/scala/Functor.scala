package scalaFP

@meta.typeclass
trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def mapConst[A, B](fa: F[A])(b: B): F[B]
}

object Functor {
  def fromMap[F[_]](map: _Map[F]): Functor[F] = {
    val x = new _MapConst[F] {
      def mapConst[A, B](fa: F[A])(x: B): F[B] = map(fa)(_ => x)
    }
    Functor(map, x)
  }
}

trait FunctorModule {
  implicit class FunctorSyntax[F[_], A](self: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map(self)(f)
    def mapConst[B](x: B): F[B] = F.mapConst(self)(x)
    def void[B]: F[Unit] = mapConst(())
  }
}
