package scalaFP

trait IsFn[P[_, _]] extends Choice[P] {
  def fromFn[A, B](f: A => B): P[A, B]
  def toFn[A, B](f: P[A, B]): A => B

  override def left[A, B, C](p: P[A, B]): P[Either[A, C], Either[B, C]] = fromFn(_.left.map(toFn(p)))
  override def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]] = fromFn(_.right.map(toFn(p)))
  override def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D) = fromFn(toFn(p).compose(f).andThen(g))
}

object IsFn {
  def apply[P[_, _]](implicit e: IIsFn[P]) = e.run

  trait From[P[_, _]] extends IsFn[P] with Choice.From[P] {
    def isFnDelegate: IsFn[P]
    override def choiceDelegate: Choice[P] = isFnDelegate
    override  def fromFn[A, B](f: A => B): P[A, B] = isFnDelegate.fromFn(f)
    override  def toFn[A, B](f: P[A, B]): A => B = isFnDelegate.toFn(f)
  }
}
