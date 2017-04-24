package scalaFP

trait BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {
  trait BH0 extends BH1 {
    implicit def applyFunctor[F[_]](implicit e: Apply[F]): Functor[F] = e.toFunctor
    implicit def hasApplyFunctor[F[_], A](implicit e: Has[Apply[F], A]): Has[Functor[F], A] =
      e.upcast(Apply.toFunctor)

    implicit def applicativeApply[F[_]](implicit e: Applicative[F]): Apply[F] = e.toApply
    implicit def hasApplicativeApply[F[_], A](implicit e: Has[Applicative[F], A]): Has[Apply[F], A] =
      e.upcast(Applicative.toApply)

    implicit def monadApplicative[F[_]](implicit e: Monad[F]): Applicative[F] = e.toApplicative
    implicit def hasMonadApplicative[F[_], A](implicit e: Has[Monad[F], A]): Has[Applicative[F], A] =
      e.upcast(Monad.diamondApplicative)

    implicit def monadBind[F[_]](implicit e: Monad[F]): Bind[F] = e.toBind
    implicit def hasMonadBind[F[_], A](implicit e: Has[Monad[F], A]): Has[Bind[F], A] =
      e.upcast(Monad.diamondBind)

    implicit def naperianMonad[F[_]](implicit e: Naperian[F]): Monad[F] = e.toMonad
    implicit def hasNaperianMonad[F[_], A](implicit e: Has[Naperian[F], A]): Has[Monad[F], A] =
      e.upcast(Naperian.toMonad)

    implicit def strongProfunctor[P[_, _]](implicit e: Strong[P]): Profunctor[P] = e.toProfunctor
    implicit def hasStrongProfunctor[P[_, _], A](implicit e: Has[Strong[P], A]): Has[Profunctor[P], A] =
      e.upcast(Strong.toProfunctor)

  }
  trait BH1 extends BH2 {
    implicit def profunctorFunctor[P[_, _], A](implicit e: Profunctor[P]): Functor[P[A, ?]] =
      e.rightFunctor.apply

    //TODO add To class which is weaker than Has, and implement To[Profunctor] => To[Functor[P[A, ?]]]

    implicit def bindApply[F[_]](implicit e: Bind[F]): Apply[F] = e.toApply
    implicit def hasBindApply[F[_], A](implicit e: Has[Bind[F], A]): Has[Apply[F], A] =
      e.upcast(Bind.toApply)

    implicit def choiceProfunctor[P[_, _]](implicit e: Choice[P]): Profunctor[P] = e.toProfunctor
    implicit def hasChoiceProfunctor[P[_, _], A](implicit e: Has[Choice[P], A]): Has[Profunctor[P], A] =
      e.upcast(Choice.toProfunctor)
  }
  trait BH2 {
    implicit def closedProfunctor[P[_, _]](implicit e: Closed[P]): Profunctor[P] = e.toProfunctor
    implicit def hasClosedProfunctor[P[_, _], A](implicit e: Has[Closed[P], A]): Has[Profunctor[P], A] =
      e.upcast(Closed.toProfunctor)
  }
}
