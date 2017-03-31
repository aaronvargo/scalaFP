package scalaFP

trait Phantom[F[_]] extends Functor[F] with Contravariant[F] {
  def phantommap[A, B](fa: F[A]): F[B] = contramap(map(fa)((x: A) => ()))((x: B) => ())
}

object Phantom {
  def apply[F[_]](implicit e: IPhantom[F]) = e.run

  trait From[F[_]] extends Phantom[F] with Functor.From[F] with Contravariant.From[F] {
    def phantomDelegate: Phantom[F]
    def functorDelegate = phantomDelegate
    def contravariantDelegate = phantomDelegate
    override def phantommap[A, B](fa: F[A]): F[B] = phantomDelegate.phantommap(fa)
  }

  trait FromPhantommap[F[_]] extends Phantom[F] {
    override def phantommap[A, B](fa: F[A]): F[B]
    override def contramap[A, B](fa: F[A])(f: B => A): F[B] = phantommap(fa)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = phantommap(fa)
  }
}
