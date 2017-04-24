package scalaFP

/**
  * The type of positions in a Naperian container.
  * if F[A] = P => A for some P, (i.e. if F is Naperian), then Log[F] = P
  *
  * The usual log laws apply: https://cstheory.stackexchange.com/a/17014
  *
  */
abstract class Log[F[_]] { self =>
  def apply[A](fa: F[A]): A

  def contramap[G[_]](f: G ~> F): Log[G] = new Log[G] {
    def apply[A](ga: G[A]) = self(f(ga))
  }

  def natural: F ~> Identity = new (F ~> Identity) {
    def apply[A](fa: F[A]): Identity[A] = Identity(self(fa))
  }
}

object Log {
  val logIdentity: Log[Identity] = new Log[Identity] {
    def apply[A](fa: Identity[A]) = fa.run
  }
}
