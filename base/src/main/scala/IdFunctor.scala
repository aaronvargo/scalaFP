package scalaFP

// TODO "extend" MonadReader and MonadError (using members and implicits)
// Actually, maybe this should only extend applicative?
trait IdFunctor[F[_]] extends Monad[F] {
  def runIdentity[A](fa: F[A]): A
}

object IdFunctor {
  def apply[F[_]](implicit e: IIdFunctor[F]) = e.run

  trait From[F[_]] extends IdFunctor[F] with Monad.From[F] {
    def idFunctorDelegate: IdFunctor[F]
    def monadDelegate = idFunctorDelegate
    def runIdentity[A](fa: F[A]): A = idFunctorDelegate.runIdentity(fa)
  }

  trait FromRunPure[F[_]] extends IdFunctor[F] with Monad.FromBindPure[F] {
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = f(runIdentity(fa))
  }
}
