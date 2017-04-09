package scalaFP

trait Bifunctor[P[_, _]] extends Right1[Functor, P] with Left1[Functor, P] {
  def bimap[A, B, C, D](p: P[A, B])(f: A => C, g: B => D): P[C, D]

  override def rightInstance[A]: Functor[P[A, ?]] = new Functor[P[A, ?]] {
    def map[B, C](p: P[A, B])(f: B => C): P[A, C] = bimap(p)(identity, f)
  }

  override def leftInstance[A]: Functor[P[?, A]] = new Functor[P[?, A]] {
    def map[B, C](p: P[B, A])(f: B => C): P[C, A] = bimap(p)(f, identity)
  }
}

object Bifunctor {
  def apply[P[_, _]](implicit e: IBifunctor[P]): Bifunctor[P] = e.run

  trait FromLeftRight[P[_, _]] extends Bifunctor[P] {
    override def rightInstance[A]: Functor[P[A, ?]]
    override def leftInstance[A]: Functor[P[?, A]]
    override def bimap[A, B, C, D](p: P[A, B])(f: A => C, g: B => D): P[C, D] = leftInstance.map(rightInstance.map(p)(g))(f)
  }
}
