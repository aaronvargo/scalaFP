package scalaFP

trait BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {
  trait BH extends BH0 {
    // TODO Consider adding this. Would allow instances to be put in companion objects in cases where they don't overlap.
    // implicit def inst[A](implicit a: A): Inst[A] = Inst(a)
  }
  trait BH0 extends BH1 {
    implicit def any1[F[_]]: IAny1[F] = Inst(new Any1[F]{})
    implicit def any2[P[_, _]]: IAny2[P] = Inst(new Any2[P]{})
    implicit def ApplyFunctor[F[_]](implicit e: IApply[F]): IFunctor[F] = e.widen
    implicit def applicativeApply[F[_]](implicit e: IApplicative[F]): IApply[F] = e.widen
    implicit def monadApplicative[F[_]](implicit e: IMonad[F]): IApplicative[F] = e.widen
    implicit def monadBind[F[_]](implicit e: IMonad[F]): IBind[F] = e.widen
    implicit def idfunctorMonad[S, F[_]](implicit e: IIdFunctor[F]): IMonad[F] = e.widen
    implicit def phantomContravariant[F[_]](implicit e: IPhantom[F]): IContravariant[F] = e.widen
    implicit def contrapplicativePhantom[F[_]](implicit e: IContrapplicative[F]): IPhantom[F] = e.widen
    implicit def choiceProfunctor[P[_, _]](implicit e: IChoice[P]): IProfunctor[P] = e.widen
    implicit def isFnChoice[P[_, _]](implicit e: IIsFn[P]): IChoice[P] = e.widen
  }
  trait BH1 extends BH2 {
    implicit def phantomFunctor[F[_]](implicit e: IPhantom[F]): IFunctor[F] = e.widen
    implicit def bindApply[F[_]](implicit e: IBind[F]): IApply[F] = e.widen
    implicit def contrapplicativeApplicative[F[_]](implicit e: IContrapplicative[F]): IApplicative[F] = e.widen
    implicit def monadReaderMonad[S, F[_]](implicit e: IMonadReader[S, F]): IMonad[F] = e.widen
    implicit def constFunctorPhantom[S, F[_]](implicit e: IConstFunctor[S, F]): IPhantom[F] = e.widen
  }
  trait BH2 {
    implicit def profunctorFunctor[P[_, _], A](implicit e: IProfunctor[P]): IFunctor[P[A, ?]] = Inst(e.run.functor)
    implicit def monadErrorMonad[S, F[_]](implicit e: IMonadError[S, F]): IMonad[F] = e.widen
  }
}
