package scalaFP

trait StdModule {
  implicit val function1IsFn: IIsFn[Function1] =
    Inst(new IsFn[Function1] {
           def fromFn[A, B](f: A => B): A => B = f
           def toFn[A, B](f: A => B): A => B = f
         })

  implicit def function1MonadReader[S]: IMonadReader[S, S => ?] =
    Inst(new MonadReader[S, S => ?] {
           def bind[A, B](fa: S => A)(f: A => (S => B)): S => B = s => f(fa(s))(s)
           def pure[A](a: A): S => A = _ => a
           def ask = identity
           def local[A](fa: S => A)(f: S => S) = f.andThen(fa)
         })

  implicit def tuple2Bifunctor: IBifunctor[Tuple2] = Inst(new Bifunctor[Tuple2] {
    def bimap[A, B, C, D](p: (A, B))(f: A => C, g: B => D): (C, D) = {
      val (x, y) = p;
      (f(x), g(y))
    }
  })

  implicit def eitherBifunctor: IBifunctor[Either] =
    Inst(new Bifunctor[Either] {
           override def bimap[A, B, C, D](p: Either[A, B])(f: A => C, g: B => D): Either[C, D] = p.left.map(f).right.map(g)
         })

  implicit class StdSyntax[A](self: A) {
    def left[B]: Either[A, B] = Left(self)
    def right[B]: Either[B, A] = Right(self)
  }
}
