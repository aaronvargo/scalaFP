package scalaFP

trait Functor[F[_]] extends Any1[F] with Functor.Map[F] with Functor.MapConst[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B]
  override def mapConst[A, B](fa: F[A], x: B): F[B] = map(fa)(_ => x)
}

object Functor {
  def apply[F[_]](implicit e: IFunctor[F]) = e.run

  trait Map[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait MapConst[F[_]] {
    def mapConst[A, B](fa: F[A], x: B): F[B]
  }

  trait From[F[_]] extends Functor[F]  {
    def functorDelegate: Functor[F]
    override def map[A, B](fa: F[A])(f: A => B) = functorDelegate.map(fa)(f)
    override def mapConst[A, B](fa: F[A], x: B): F[B] = functorDelegate.mapConst(fa, x)
  }

  def construct[F[_]](_map: Map[F], _mapConst: MapConst[F]): Functor[F] = new Functor[F] {
    def map[A, B](fa: F[A])(f: A => B): F[B] = _map.map(fa)(f)
    override def mapConst[A, B](fa: F[A], x: B): F[B] = _mapConst.mapConst(fa, x)
  }

  def copy[F[_]](x: Functor[F])(_map: Map[F] = x, _mapConst: MapConst[F] = x): Functor[F] =
    construct(_map, _mapConst)

  def mapL[F[_]]: Lens_[Functor[F], Map[F]] = Optic.superLens(x => copy(_)(_map = x))
  def mapConstL[F[_]]: Lens_[Functor[F], MapConst[F]] = Optic.superLens(x => copy(_)(_mapConst = x))

  def fromMap[F[_]](_map: Map[F]): Functor[F] = new Functor[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] = _map.map(fa)(f)
  }
}
