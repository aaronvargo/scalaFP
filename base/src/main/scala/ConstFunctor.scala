package scalaFP

trait ConstFunctor[V, F[_]] extends Phantom[F] {
  def runConst[A](fa: F[A]): V
  def mkConst[A](v: V): F[A]
  override def phantommap[A, B](fa: F[A]): F[B] = mkConst(runConst(fa))
}

object ConstFunctor {
  def apply[V, F[_]](implicit e: IConstFunctor[V, F]) = e.run

  trait From[V, F[_]] extends ConstFunctor[V, F] with Phantom.From[F] {
    def constFunctorDelegate: ConstFunctor[V, F]
    override def phantomDelegate: Phantom[F] = constFunctorDelegate
    override def runConst[A](fa: F[A]): V = constFunctorDelegate.runConst(fa)
    override def mkConst[A](v: V): F[A] = constFunctorDelegate.mkConst(v)
  }
}
