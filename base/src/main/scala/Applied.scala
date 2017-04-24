package scalaFP

@meta.newtype
object AppliedType {
  type Applied[A, F[_]] = F[A]

  def functor1[A]: Functor1[Applied[A, ?[_]]] = new Functor1._Map1[Applied[A, ?[_]]] {
    override def map1[F[_], G[_]](x: Applied[A, F])(f: F ~> G): Applied[A, G] = f(x)
  }.mkFunctor1
}

trait AppliedModule extends AppliedType.TopLevel {
  implicit def appliedFunctor1[A]: Functor1[Applied[A, ?[_]]] = Applied.functor1
}
