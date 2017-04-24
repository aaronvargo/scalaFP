package scalaFP

@meta.typeclass
trait Monad[F[_]] {
  val toBind: Bind[F]
  val toApplicative: Applicative[F]
}

object Monad {

  def diamondBind[F[_], A: Has[Monad[F], ?]]: Lens_[A, Bind[F]] =
    Lens.diamond[Apply[F]](toBind, toApplicative)

  def diamondApplicative[F[_], A: Has[Monad[F], ?]]: Lens_[A, Applicative[F]] =
    Lens.diamond[Apply[F]](toApplicative, toBind)

  def fromToBindPure[F[_]](bind: Bind[F], pure: Applicative._Pure[F]): Monad[F] =
    Monad(bind, Applicative(pure, bind.toApply))

  def fromBindPure[F[_]](bind: Bind._Bind[F], pure: Applicative._Pure[F]): Monad[F] = {
    val x = new Functor._Map[F] {
      def map[A, B](fa: F[A])(f: A => B): F[B] = bind(fa)(f.andThen(pure.apply))
    }
    fromToBindPure(Bind.fromBindMap(bind, x), pure)
  }

  def fromPureMapJoin[F[_]](pure: Applicative._Pure[F], map: Functor._Map[F], join: Bind._Join[F]): Monad[F] = {
    fromToBindPure(Bind.fromJoinMap(join, map), pure)
  }

}
