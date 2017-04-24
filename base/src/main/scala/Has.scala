package scalaFP

case class Has[B, A](run: Lens_[A, B]) {
  def upcast[C](l: Lens_[B, C]): Has[C, A] = Has(run.composeL(l))
}

object Has {
  implicit def hasRefl[A]: Has[A, A] = Has(Equality.refl)

  def lens[B, A](implicit e: Has[B, A]) = e.run
  def view[B, A: Has[B, ?]](a: A): B = lens[B, A].apply(a)
}

trait HasModule {
  implicit class HasSyntax[A](self: A) {
    def has[B](implicit e: Has[B, A]): B = Has.view(self)
  }
}
