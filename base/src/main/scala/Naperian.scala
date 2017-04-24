package scalaFP

/**
  * A Naperian functor is a container with a fixed size, such as
  * `data Triple a = Triple a a a`, or Stream, but not List or Option.
  *
  * All Naperian functors are equivalent to a functor (P => ?), for some P,
  * where P is the type of positions in the container.
  * e.g. for Triple, P = 3 and for Stream, P = Nat.
  *
  * The equivalence can be witnessed in the general case by selecting
  * P = Log F = ∀a. F a -> a
  * i.e. a position is just a function which gets the element at that position.
  *
  * Note that tabulate and gcotraverse are equivalent, but the former is
  * inefficient for containers without random access.
  *
  * All Naperian functors are diagnolization monads/zip applicatives
  *
  */
@meta.typeclass
trait Naperian[F[_]] {
  def gcotraverse[G[_[_]]: Functor1, A](gf: G[F])(f: G[Identity] => A): F[A]
  def gdistribute[G[_[_]]: Functor1](gf: G[F]): F[G[Identity]]
  def tabulate[A](f: Log[F] => A): F[A]
  def cotraverse[G[_]: Functor, A, B](gfa: G[F[A]])(f: G[A] => B): F[B]
  def distribute[G[_]: Functor, A](gf: G[F[A]]): F[G[A]]
  val toMonad: Monad[F]
}

object Naperian {

  //TODO from gidstribute and functor

  def fromTabulate[F[_]](tabulate: _Tabulate[F]): Naperian[F] = {
    val x = new _Gcotraverse[F] {
      def gcotraverse[G[_[_]]: Functor1, A](gf: G[F])(f: G[Identity] => A): F[A] =
        tabulate(p => f(gf.map1(p.natural)))
    }
    fromGcotraverse(x).oset(tabulate)(Naperian.tabulate)
  }

  def fromGcotraverse[F[_]](gcotraverse: _Gcotraverse[F]): Naperian[F] =
    {
      val x = new _Gdistribute[F] with _Distribute[F] with _Cotraverse[F] with _Tabulate[F] with Applicative._Pure[F] with Bind._Bind[F] {

        def gdistribute[G[_[_]]](gf: G[F])(implicit e: Functor1[G]): F[G[Identity]] =
          gcotraverse(gf)(identity)

        def cotraverse[G[_]: Functor, A, B](gfa: G[F[A]])(f: G[A] => B): F[B] = {
          type H[X[_]] = G[X[A]]
          def hFunctor1: Functor1[H] = new Functor1._Map1[H] {
            def map1[P[_], Q[_]](x: G[P[A]])(f: P ~> Q): G[Q[A]] = x.map(f(_))
          }.mkFunctor1

          gcotraverse[H, B](gfa)(f.compose(_.map(_.run)))(hFunctor1)
        }

        def distribute[G[_]: Functor, A](gf: G[F[A]]): F[G[A]] = cotraverse(gf)(identity)

        def tabulate[A](f: Log[F] => A): F[A] = {
          type G[X[_]] = Log[X] => A
          def gFunctor1: Functor1[G] = new Functor1._Map1[G] {
            def map1[P[_], Q[_]](x: Log[P] => A)(f: P ~> Q): Log[Q] => A =
              x.compose(_.contramap(f))
          }.mkFunctor1
          gcotraverse[G, A](f)(g => g(Log.logIdentity))(gFunctor1)
        }

        def pure[A](a: A): F[A] = distribute[ConstAlias.Const[A, ?], Unit](a)(ConstAlias.functor)

        def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = {
          type G[P[_]] = (P[A], A => P[B])
          def gFunctor1 = new Functor1._Map1[G] {
            def map1[P[_], Q[_]](x: G[P])(f: P ~> Q): G[Q] = {
              val (pa, apb) = x
              (f(pa), apb.andThen(f(_)))
            }
          }.mkFunctor1

          gcotraverse[G, B]((fa, f)){ case (ia, aib) => ia.bind(aib).run }(gFunctor1)
        }

      }
      Naperian(gcotraverse, x, x, x, x, Monad.fromBindPure(x, x))
    }
}

trait NaperianModule {
  implicit class NaperianSyntax1[F[_], G[_], A](val self: G[F[A]])(implicit F: Naperian[F], G: Functor[G]) {
    def cotraverse[B](f: G[A] => B): F[B] = F.cotraverse(self)(f)
    def distribute: F[G[A]] = F.distribute(self)
  }
}
