package scalaFP

trait ScalaFP
    extends BaseHierarchy
    with InstModule
    with LeibnizModule
    with IdentityModule
    with ConstModule
    with ExchangeModule
    with IsFnModule
    with OpticModule
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
  type IProfunctor[P[_, _]] = Inst[Profunctor[P]]
  type IChoice[P[_, _]] = Inst[Choice[P]]
  type IIsFn[P[_, _]] = Inst[IsFn[P]]
}
