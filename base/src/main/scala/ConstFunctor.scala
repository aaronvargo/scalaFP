package scalaFP

trait ConstFunctor[V, F[_]] extends Phantom[F] {
  def runConst[A](fa: F[A]): V
  def mkConst[A](v: V): F[A]
}

object ConstFunctor {
  def apply[V, F[_]](implicit e: IConstFunctor[V, F]) = e.run

  trait From[V, F[_]] extends ConstFunctor[V, F] with Phantom.From[F] {
    def constFunctorDelegate: ConstFunctor[V, F]
    def phantomDelegate = constFunctorDelegate
    def runConst[A](fa: F[A]): V = constFunctorDelegate.runConst(fa)
    def mkConst[A](v: V): F[A] = constFunctorDelegate.mkConst(v)
  }

  trait FromRunMk[V, F[_]] extends ConstFunctor[V, F] with Phantom.FromPhantommap[F] {
    override def phantommap[A, B](fa: F[A]): F[B] = mkConst(runConst(fa))
  }
}
