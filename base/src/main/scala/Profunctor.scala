package scalaFP

trait Profunctor[P[_, _]] extends Right1[Functor, P] with Left1[Contravariant, P] {
  def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]

  override def rightInstance[A]: Functor[P[A, ?]] = new Functor[P[A, ?]] {
    def map[B, C](p: P[A, B])(f: B => C): P[A, C] = dimap(p)(identity, f)
  }

  override def leftInstance[A]: Contravariant[P[?, A]] = new Contravariant[P[?, A]] {
    override def contramap[B, C](p: P[B, A])(f: C => B): P[C, A] = dimap(p)(f, identity)
  }
}

object Profunctor {
  def apply[P[_, _]](implicit e: IProfunctor[P]) = e.run

  trait From[P[_, _]] extends Profunctor[P] {
    def profunctorDelegate: Profunctor[P]
    override def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = profunctorDelegate.dimap(p)(f, g)
    override def rightInstance[A]: Functor[P[A, ?]] = profunctorDelegate.rightInstance
    override def leftInstance[A]: Contravariant[P[?, A]] = profunctorDelegate.leftInstance
  }

  trait FromLeftRight[P[_, _]] extends Profunctor[P] {
    override def leftInstance[A]: Contravariant[P[?, A]]
    override def rightInstance[A]: Functor[P[A, ?]]
    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = rightInstance.map(leftInstance.contramap(p)(f))(g)
  }
}
