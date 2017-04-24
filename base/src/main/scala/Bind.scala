package scalaFP

@meta.typeclass
trait Bind[F[_]] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]): F[A]
  val toApply: Apply[F]
}

object Bind {
  def fromBindMap[F[_]](bind: _Bind[F], map: Functor._Map[F]): Bind[F] = {
    val x = new Apply._Ap[F] with _Join[F] {
      def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = bind(fab)(map(fa))
      def join[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)
    }
    Bind(bind, x, Apply.fromApMap(x, map))
  }

  def fromJoinMap[F[_]](join: _Join[F], map: Functor._Map[F]): Bind[F] = {
    val x = new _Bind[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
    }
    fromBindMap(x, map).oset(join)(Bind.join)
  }
}

trait BindModule {
  implicit class BindSyntax[F[_], A](self: F[A])(implicit F: Bind[F]) {
    def bind[B](f: A => F[B]): F[B] = F.bind(self)(f)
  }

  implicit class BindSyntax1[F[_], A](self: F[F[A]])(implicit F: Bind[F]) {
    def join: F[A] = F.join(self)
  }
}
