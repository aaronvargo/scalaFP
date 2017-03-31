package scalaFP

trait IsFn[P[_, _]] extends Choice[P] {
  def fromFn[A, B](f: A => B): P[A, B]
  def toFn[A, B](f: P[A, B]): A => B
}

object IsFn {
  def apply[P[_, _]](implicit e: IIsFn[P]) = e.run

  trait From[P[_, _]] extends IsFn[P] with Choice.From[P] {
    def isFnDelegate: IsFn[P]
    def choiceDelegate = isFnDelegate
    def fromFn[A, B](f: A => B): P[A, B] = isFnDelegate.fromFn(f)
    def toFn[A, B](f: P[A, B]): A => B = isFnDelegate.toFn(f)
  }

  trait FromFromTo[P[_, _]] extends IsFn[P] {
    def left[A, B, C](p: P[A, B]): P[Either[A, C], Either[B, C]] = fromFn(_.left.map(toFn(p)))
    def right[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]] = fromFn(_.right.map(toFn(p)))
    def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D) = fromFn(toFn(p).compose(f).andThen(g))
  }
}

trait IsFnModule {
  implicit val fnIsFn: IIsFn[Function1] =
    Inst(new IsFn.FromFromTo[Function1] {
      def fromFn[A, B](f: A => B): A => B = f
      def toFn[A, B](f: A => B): A => B = f
    })
}
