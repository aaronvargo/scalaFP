package scalaFP

trait Profunctor[P[_, _]] {
  def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]

  def functor[A]: Functor[P[A, ?]] = new Functor[P[A, ?]] {
    def map[B, C](p: P[A, B])(f: B => C): P[A, C] = rmap(p)(f)
  }

  def lmap[A, B, C](p: P[A, B])(f: C => A): P[C, B] = dimap(p)(f, identity)
  def rmap[A, B, C](p: P[A, B])(f: B => C): P[A, C] = dimap(p)(identity, f)
}

object Profunctor {
  def apply[P[_, _]](implicit e: IProfunctor[P]) = e.run

  trait From[P[_, _]] extends Profunctor[P] {
    def profunctorDelegate: Profunctor[P]
    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = profunctorDelegate.dimap(p)(f, g)
    override def functor[A]: Functor[P[A, ?]] = profunctorDelegate.functor
    override def lmap[A, B, C](p: P[A, B])(f: C => A): P[C, B] = profunctorDelegate.lmap(p)(f)
    override def rmap[A, B, C](p: P[A, B])(f: B => C): P[A, C] = profunctorDelegate.rmap(p)(f)
  }

  trait FromFunctorLMap[P[_, _]] extends Profunctor[P] {
    override def functor[A]: Functor[P[A, ?]]
    override def lmap[A, B, C](p: P[A, B])(f: C => A): P[C, B]

    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = functor.map(lmap(p)(f))(g)
    override def rmap[A, B, C](p: P[A, B])(f: B => C): P[A, C] = functor.map(p)(f)
  }
}
