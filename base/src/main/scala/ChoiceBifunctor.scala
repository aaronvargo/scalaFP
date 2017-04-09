package scalaFP

trait ChoiceBifunctor[P[_, _]] extends Choice[P] with Profunctor.FromLeftRight[P] with Bifunctor.FromLeftRight[P] with Left1[Phantom, P] {
  def dualmap[A, B, C, D](p: P[A, B])(f: B => D): P[C, D]

  override def left[A, B, C](p: P[A, B]): P[Either[A, C], Either[B, C]] = dualmap(p)(_.left)
  override def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]] = dualmap(p)(_.right)

  override def leftInstance[A]: Phantom[P[?, A]] = new Phantom[P[?, A]] {
    def phantommap[B, C](p: P[B, A]): P[C, A] = dualmap(p)(identity)
  }

  override def rightInstance[A]: Functor[P[A, ?]] = new Functor[P[A, ?]] {
    def map[B, C](p: P[A, B])(f: B => C): P[A, C] = dualmap(p)(f)
  }
}

object ChoiceBifunctor {
  def apply[P[_, _]](implicit e: IChoiceBifunctor[P]) = e.run

  trait FromLeftRight[P[_, _]] extends ChoiceBifunctor[P] {
    override def leftInstance[A]: Phantom[P[?, A]]
    override def rightInstance[A]: Functor[P[A, ?]]
    def dualmap[A, B, C, D](p: P[A, B])(f: B => D): P[C, D] = leftInstance.phantommap(rightInstance.map(p)(f))
  }
}
