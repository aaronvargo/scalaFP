package scalaFP

trait Base
    extends BaseHierarchy
    with InstModule
    with IdentityModule
    with ConstModule
    with OpticModule
    with StdModule
    with MiscSyntax {

  type IAny1[F[_]] = Inst[Any1[F]]
  type IFunctor[F[_]] = Inst[Functor[F]]
  type IApply[F[_]] = Inst[Apply[F]]
  type IBind[F[_]] = Inst[Bind[F]]
  type IContravariant[F[_]] = Inst[Contravariant[F]]
  type IPhantom[F[_]] = Inst[Phantom[F]]
  type IApplicative[F[_]] = Inst[Applicative[F]]
  type IMonad[F[_]] = Inst[Monad[F]]
  type IMonadReader[S, F[_]] = Inst[MonadReader[S, F]]
  type IMonadError[S, F[_]] = Inst[MonadError[S, F]]
  type IContrapplicative[F[_]] = Inst[Contrapplicative[F]]
  type IConstFunctor[V, F[_]] = Inst[ConstFunctor[V, F]]
  type IIdFunctor[F[_]] = Inst[IdFunctor[F]]

  type IAny2[P[_, _]] = Inst[Any2[P]]
  type IRight1[C[_[_]], P[_, _]] = Inst[Right1[C, P]]
  type ILeft1[C[_[_]], P[_, _]] = Inst[Left1[C, P]]
  type IBifunctor[P[_, _]] = Inst[Bifunctor[P]]
  type IProfunctor[P[_, _]] = Inst[Profunctor[P]]
  type IChoice[P[_, _]] = Inst[Choice[P]]
  type IIsFn[P[_, _]] = Inst[IsFn[P]]
  type IChoiceBifunctor[P[_, _]] = Inst[ChoiceBifunctor[P]]

  type ===[A, B] = Inst[Equality_[A, B]]
}
