package scalaFP

// TODO "extend" MonadReader and MonadError (using members and implicits)
// Actually, maybe this should only extend applicative?
trait IdFunctor[F[_]] extends Monad[F] {
  def runIdentity[A](fa: F[A]): A
  override def pure[A](a: A): F[A]
  override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = f(runIdentity(fa))
}

object IdFunctor {
  def apply[F[_]](implicit e: IIdFunctor[F]) = e.run

  trait From[F[_]] extends IdFunctor[F] with Monad.From[F] {
    def idFunctorDelegate: IdFunctor[F]
    override def monadDelegate: Monad[F] = idFunctorDelegate
    override def runIdentity[A](fa: F[A]): A = idFunctorDelegate.runIdentity(fa)
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = idFunctorDelegate.bind(fa)(f)
  }
}
