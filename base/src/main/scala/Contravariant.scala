package scalaFP

trait Contravariant[F[_]] extends Any1[F] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}

object Contravariant {
  def apply[F[_]](implicit e: IContravariant[F]) = e.run

  trait From[F[_]] extends Contravariant[F] {
    def contravariantDelegate: Contravariant[F]
    def contramap[A, B](fa: F[A])(f: B => A): F[B] = contravariantDelegate.contramap(fa)(f)
  }
}
