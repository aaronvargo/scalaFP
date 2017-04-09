package scalaFP

trait Phantom[F[_]] extends Functor[F] with Contravariant[F] {
  def phantommap[A, B](fa: F[A]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = phantommap(fa)
  override def contramap[A, B](fa: F[A])(f: B => A): F[B] = phantommap(fa)
}

object Phantom {
  def apply[F[_]](implicit e: IPhantom[F]) = e.run

  trait From[F[_]] extends Phantom[F] with Functor.From[F] with Contravariant.From[F] {
    def phantomDelegate: Phantom[F]
    override def functorDelegate: Functor[F] = phantomDelegate
    override def contravariantDelegate: Contravariant[F] = phantomDelegate
    override def phantommap[A, B](fa: F[A]): F[B] = phantomDelegate.phantommap(fa)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = phantomDelegate.map(fa)(f)
    override def contramap[A, B](fa: F[A])(f: B => A): F[B] = phantomDelegate.contramap(fa)(f)
  }

  trait FromMapContramap[F[_]] extends Phantom[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B]
    override def contramap[A, B](fa: F[A])(f: B => A): F[B]
    override def phantommap[A, B](fa: F[A]): F[B] = contramap(map(fa)((x: A) => ()))((x: B) => ())
  }
}
