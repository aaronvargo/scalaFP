package scalaFP

// mkFunctor1 needs to be a method for type inference to work
@meta.classyLenses
case class Functor1[F[_[_]]](map1: Functor1._Map1[F])

object Functor1 {
  def apply[F[_[_]]](implicit e: Functor1[F]) = e

  trait _Map1[F[_[_]]] {
    def map1[A[_], B[_]](fa: F[A])(f: A ~> B): F[B]

    final def mkFunctor1: Functor1[F] = Functor1(this)
  }
}

trait Functor1Module {
  implicit class Functor1Syntax[F[_[_]], A[_]](self: F[A])(implicit F: Functor1[F]) {
    def map1[B[_]](f: A ~> B): F[B] = F.map1.map1(self)(f)
  }
}
