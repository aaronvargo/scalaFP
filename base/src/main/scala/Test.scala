import scalaFP._

object Test {
  def foo[F[_]: IIdFunctor] = {
    Functor[F]
    Apply[F]
    Applicative[F]
    Bind[F]
    Monad[F]
    IdFunctor[F]
  }

  def bar[F[_]: IMonadReader[Int, ?[_]] : IMonadError[String, ?[_]]] = {
    Functor[F]
    Apply[F]
    Applicative[F]
    Bind[F]
    Monad[F]
    MonadReader[Int, F]
    MonadError[String, F]
  }

  def baz[P[_, _]: IIsFn] = {
    Functor[P[Int, ?]]
    Profunctor[P]
    Choice[P]
  }
}
