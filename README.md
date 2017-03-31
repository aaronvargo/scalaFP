Experiments in modern FP library design in Scala, including:

- [scato](https://github.com/aloiscochard/scato) style typeclasses, but which still utilize inheritance
- coercible newtypes, based on the tagged type trick found in scalaz
- [lens](https://hackage.haskell.org/package/lens) style optics, based on variance-induced subtyping

Here's a list of some interesting things to look at:

- [Newtypes are encoded with the tagged-type trick](https://github.com/aaronvargo/scalaFP/blob/master/base/src/main/scala/Const.scala)

- The `Inst` newtype, which is equivalent to the Identity newtype, is
used to remove subtyping between typeclasses, so that a scato-style encoding can
be used. Since `Inst` is invariant, `Inst[Monad[F]]` and `Inst[Applicative[F]]`
(aliased to `IMonad[F]` and `IApplicative[F]` respectively), for example, have
no subtype relation, even though `Monad[F]` is defined as a subtype of `Applicative[F]`

- `IProfunctor[P]` "extends" `∀A. IFunctor[P[A, ?]]`:

  ~~~scala
  implicit def profunctorFunctor[P[_, _], A](implicit e: IProfunctor[P]): IFunctor[P[A, ?]] = Inst(e.run.functor)
  ~~~

- lens style optics are all encoded in terms of a single type:

  ~~~scala
  abstract class Optic[-PC[_[_, _]], -FC[_[_]], S, T, A, B] {
    def explicitTransform[P[_, _], F[_]](f: P[A, F[B]], P: PC[P], F: FC[F]): P[S, F[T]]
  }

  type Equality[S, T, A, B] = Optic[Any2, Any1, S, T, A, B]
  type Iso[S, T, A, B] = Optic[Profunctor, Functor, S, T, A, B]
  type AnIso[S, T, A, B] = Optic[Profunctor, IdFunctor, S, T, A, B]
  type Prism[S, T, A, B] = Optic[Choice, Applicative, S, T, A, B]
  type LensLike[-C[_[_]], S, T, A, B] = Optic[IsFn, C, S, T, A, B]
  type Getting[R, S, T, A, B] = LensLike[ConstFunctor[R, ?[_]], S, T, A, B]
  type Getter[S, T, A, B] = LensLike[Phantom, S, T, A, B]
  type Setter[S, T, A, B] = LensLike[IdFunctor, S, T, A, B]
  type Fold[S, T, A, B] = LensLike[Contrapplicative, S, T, A, B]
  type Traversal[S, T, A, B] = LensLike[Applicative, S, T, A, B]
  type Lens[S, T, A, B] = LensLike[Functor, S, T, A, B]
  ~~~

  The proper subtype relations between these types fall out of the contravariance
  of PC and FC. This makes it possible to, for example, have a single compose
  function which just does the right thing, as in the lens library. 

- [It's not too hard to define lenses for typeclasses](https://github.com/aaronvargo/scalaFP/blob/master/base/src/main/scala/Apply.scala),
though I'm not sure how useful they are. But in any case, though typeclasses are
defined with inheritance, it's also possible to construct them using composition
and value level functions.
