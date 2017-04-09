package scalaFP

trait BaseHierarchy extends BaseHierarchy.BH

object BaseHierarchy {
  trait BH extends BH0 {
    implicit def rightInstance[C[_[_]], P[_, _], A](implicit e: IRight1[C, P]): Inst[C[P[A, ?]]] = Inst(e.run.rightInstance)

    /* TODO repeat entire hierarchy for Right and Left?
       IRight[Apply] => IRight[Functor], etc?
    */

    //TODO Find a way to add default instances such as (Bind, Applicative) => Monad
  }
  trait BH0 extends BH1 {
    implicit def equalityInst[A]: A === A = Inst(Optic.id)
    implicit def any1[F[_]]: IAny1[F] = Inst(new Any1[F]{})
    implicit def any2[P[_, _]]: IAny2[P] = Inst(new Any2[P]{})
    implicit def applyFunctor[F[_]](implicit e: IApply[F]): IFunctor[F] = e.widen
    implicit def applicativeApply[F[_]](implicit e: IApplicative[F]): IApply[F] = e.widen
    implicit def monadApplicative[F[_]](implicit e: IMonad[F]): IApplicative[F] = e.widen
    implicit def monadBind[F[_]](implicit e: IMonad[F]): IBind[F] = e.widen
    implicit def idfunctorMonad[S, F[_]](implicit e: IIdFunctor[F]): IMonad[F] = e.widen
    implicit def phantomContravariant[F[_]](implicit e: IPhantom[F]): IContravariant[F] = e.widen
    implicit def contrapplicativePhantom[F[_]](implicit e: IContrapplicative[F]): IPhantom[F] = e.widen
    implicit def choiceProfunctor[P[_, _]](implicit e: IChoice[P]): IProfunctor[P] = e.widen
    implicit def isFnChoice[P[_, _]](implicit e: IIsFn[P]): IChoice[P] = e.widen
    implicit def profunctorRight[P[_, _]](implicit e: IProfunctor[P]): IRight1[Functor, P] = e.widen
    implicit def profunctorLeft[P[_, _]](implicit e: IProfunctor[P]): ILeft1[Contravariant, P] = e.widen
    implicit def choiceBifunctorBifunctor[P[_, _]](implicit e: IChoiceBifunctor[P]): IBifunctor[P] = e.widen
  }
  trait BH1 extends BH2 {
    implicit def phantomFunctor[F[_]](implicit e: IPhantom[F]): IFunctor[F] = e.widen
    implicit def bindApply[F[_]](implicit e: IBind[F]): IApply[F] = e.widen
    implicit def contrapplicativeApplicative[F[_]](implicit e: IContrapplicative[F]): IApplicative[F] = e.widen
    implicit def monadReaderMonad[S, F[_]](implicit e: IMonadReader[S, F]): IMonad[F] = e.widen
    implicit def constFunctorPhantom[S, F[_]](implicit e: IConstFunctor[S, F]): IPhantom[F] = e.widen
    implicit def bifunctorRight[P[_, _]](implicit e: IBifunctor[P]): IRight1[Functor, P] = e.widen
    implicit def bifunctorLeft[P[_, _]](implicit e: IBifunctor[P]): ILeft1[Functor, P] = e.widen
    implicit def choiceBifunctorChoice[P[_, _]](implicit e: IChoiceBifunctor[P]): IChoice[P] = e.widen
    implicit def choiceBifunctorLeft[P[_, _]](implicit e: IChoiceBifunctor[P]): ILeft1[Phantom, P] = e.widen
  }
  trait BH2 {
    implicit def monadErrorMonad[S, F[_]](implicit e: IMonadError[S, F]): IMonad[F] = e.widen
  }
}
