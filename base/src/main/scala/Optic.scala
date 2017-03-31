package scalaFP

abstract class Optic[-PC[_[_, _]], -FC[_[_]], S, T, A, B] { self =>
  def explicitTransform[P[_, _], F[_]](f: P[A, F[B]], P: PC[P], F: FC[F]): P[S, F[T]]

  final def compose[PC1[P[_, _]] <: PC[P], FC1[F[_]] <: FC[F], C, D]
      (o: Optic[PC1, FC1, A, B, C, D]): Optic[PC1, FC1, S, T, C, D] = Optic.compose(self, o)
}

object Optic {
  /** Construct an Optic */
  abstract class _Optic[PC[_[_, _]], FC[_[_]], S, T, A, B] {
    def transform[P[_, _], F[_]](f: P[A, F[B]])(implicit P: Inst[PC[P]], F: Inst[FC[F]]): P[S, F[T]]

    final def optic: Optic[PC, FC, S, T, A, B] = new Optic[PC, FC, S, T, A, B] {
      def explicitTransform[P[_, _], F[_]](f: P[A, F[B]], P: PC[P], F: FC[F]): P[S, F[T]] =
        transform[P, F](f)(Inst(P), Inst(F))
    }
  }

  /** Construct a LensLike */
  abstract class _LensLike[C[_[_]], S, T, A, B] {
    def overF[F[_]](f: A => F[B])(s: S)(implicit F: Inst[C[F]]): F[T]

    final def optic: LensLike[C, S, T, A, B] =
      new _Optic[IsFn, C, S, T, A, B] {
        def transform[P[_, _], F[_]](f: P[A, F[B]])(implicit P: Inst[IsFn[P]], F: Inst[C[F]]): P[S, F[T]] =
          IsFn[P].fromFn(overF(f.toFn))
      }.optic
  }

  /** Monomorphize an optic. Seems this can't be done with dot syntax. */
  def mono[PC[_[_, _]], FC[_[_]], S, A](o: Optic_[PC, FC, S, A]): Optic_[PC, FC, S, A] = o

  def id[PC[_[_, _]], FC[_[_]], S, T]: Optic[PC, FC, S, T, S, T] =
    new Optic[PC, FC, S, T, S, T] {
      def explicitTransform[P[_, _], F[_]](f: P[S, F[T]], P: PC[P], F: FC[F]): P[S, F[T]] = f
    }

  def compose[PC[_[_, _]], FC[_[_]], S, T, A, B, C, D]
    (o1: Optic[PC, FC, S, T, A, B], o2: Optic[PC, FC, A, B, C, D])
      : Optic[PC, FC, S, T, C, D] =
    new _Optic[PC, FC, S, T, C, D] {
      def transform[P[_, _], F[_]](f: P[C, F[D]])
               (implicit P: Inst[PC[P]], F: Inst[FC[F]]): P[S, F[T]] = o1.transform[P, F](o2.transform[P, F](f))
    }.optic


  def iso[S, T, A, B](f: S => A, g: B => T): Iso[S, T, A, B] =
    new _Optic[Profunctor, Functor, S, T, A, B] {
      def transform[P[_, _]: IProfunctor, F[_]: IFunctor](p: P[A, F[B]]): P[S, F[T]] = p.dimap(f, _.map(g))
    }.optic

  def getter[S, T, A, B](_get: S => A): Getter[S, T, A, B] =
    new _LensLike[Phantom, S, T, A, B] {
      def overF[F[_]: IPhantom](f: A => F[B])(s: S): F[T] =
        f(_get(s)).phantommap
    }.optic

  def setter[S, T, A, B](_modify: (A => B, S) => T): Setter[S, T, A, B] =
    new _LensLike[IdFunctor, S, T, A, B] {
      def overF[F[_]: IIdFunctor](f: A => F[B])(s: S): F[T] =
        _modify(f.andThen(_.runIdentity), s).pure[F]
    }.optic

  def lens[S, T, A, B](_get: S => A, _set: B => S => T): Lens[S, T, A, B] =
    new _LensLike[Functor, S, T, A, B] {
      def overF[F[_]: IFunctor](f: A => F[B])(s: S): F[T] = f(_get(s)).map(x => _set(x)(s))
    }.optic

  /** Construct a lens to a supertype */
  def superLens[S, T, A >: S, B](_set: B => S => T): Lens[S, T, A, B] = lens(identity, _set)

  implicit class OpticSyntax[PC[_[_, _]], FC[_[_]], S, T, A, B](self: Optic[PC, FC, S, T, A, B]) {
    //Can't be added directly to the class due to variance
    def transform[P[_, _], F[_]](f: P[A, F[B]])(implicit P: Inst[PC[P]], F: Inst[FC[F]]): P[S, F[T]] =
      self.explicitTransform[P, F](f, P.run, F.run)
  }

  implicit class AnIsoSytax[S, T, A, B](self: AnIso[S, T, A, B]) {
    def foldIso[R](f: (S => A, B => T) => R): R = {
      val x = self.transform[Exchange[A, B, ?, ?], Identity](Exchange(identity, Identity(_)))
      f(x.to, x.from.andThen(_.runIdentity))
    }
    def flipIso: Iso[B, A, T, S] = foldIso((f, g) => Optic.iso(g, f))
    def ofrom(b: B): T = self.flipIso.oview(b)
  }

  implicit class LensLikeSyntax[C[_[_]], S, T, A, B](self: LensLike[C, S, T, A, B]) {
    def ooverF[F[_]](s: S, f: A => F[B])(implicit F: Inst[C[F]]): F[T] = self.transform[Function1, F](f).apply(s)
  }

  implicit class SetterSyntax[S, T, A, B](self: Setter[S, T, A, B]) {
    def oover(s: S)(f: A => B): T = self.ooverF(s, f.andThen(Identity(_))).run
    def oset(s: S, x: B): T = oover(s)(_ => x)
  }

  implicit class GetterSyntax[S, T, A, B](self: Getter[S, T, A, B]) {
    def oview(s: S): A = self.ooverF[Const[A, ?]](s, Const(_)).runConst
  }

  def fst[A, B, C]: Lens[(A, C), (B, C), A, B] = lens(_._1, x => _.copy(_1 = x))
  def snd[A, B, C]: Lens[(A, B), (A, C), B, C] = lens(_._2, x => _.copy(_2 = x))
}

trait OpticModule {
  type Is[S, T, A, B] = Optic[Any2, Any1, S, T, A, B]
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

  type Optic_[PC[_[_, _]], FC[_[_]], S, A] = Optic[PC, FC, S, S, A, A]
  type Is_[S, A] = Is[S, S, A, A]
  type Iso_[S, A] = Iso[S, S, A, A]
  type AnIso_[S, A] = AnIso[S, S, A, A]
  type Prism_[S, A] = Prism[S, S, A, A]
  type LensLike_[-C[_[_]], S, A] = LensLike[C, S, S, A, A]
  type Getting_[R, S, A] = Getting[R, S, S, A, A]
  type Getter_[S, A] = Getter[S, S, A, A]
  type Setter_[S, A] = Setter[S, S, A, A]
  type Fold_[S, A] = Fold[S, S, A, A]
  type Traversal_[S, A] = Traversal[S, S, A, A]
  type Lens_[S, A] = Lens[S, S, A, A]

  implicit class OpticSyntax0[S](self: S) {
    def overF[C[_[_]], F[_], T, A, B](f: A => F[B])(o: LensLike[C, S, T, A, B])(implicit F: Inst[C[F]]): F[T] = o.ooverF(self, f)

    def moverF[C[_[_]], F[_], A](f: A => F[A])(o: LensLike_[C, S, A])(implicit F: Inst[C[F]]): F[S] = o.ooverF(self, f)

    def poverF[C[_[_]], F[_], T, A, B]
      (proxy: LensLike_[C, S, A])(f: A => F[B])(o: LensLike[C, S, T, A, B])(implicit F: Inst[C[F]]): F[T] = o.ooverF(self, f)

    def overF1[C[_[_]], F[_], T, A, B](o: LensLike[C, S, T, A, B])(f: A => F[B])(implicit F: Inst[C[F]]): F[T] = o.ooverF(self, f)

    /** Polymorphic over. Can't infer the argument type of the function. */
    def over[T, A, B](f: A => B)(o: Setter[S, T, A, B]): T = o.oover(self)(f)

    /** Monomorphic over. Has better inference; can infer A. */
    def mover[T, A, B](o: Setter_[S, A])(f: A => A): S = o.oover(self)(f)

    /** Polymorphic over, where a monomorphic version of the setter is also passed as a proxy to aid type inference.
      Can allow A to be inferred. For example, the following type checks:
        ((3, 2), 5).pover(fst.compose(snd))(_.toString)(fst.compose(snd))
      Though it may just be easier to add a type annotation:
        ((3, 2), 5).over((_:Int).toString)(fst.compose(snd))
    */
    def pover[T, A, B](proxy: Setter[S, S, A, A])(f: A => B)(o: Setter[S, T, A, B]): T = o.oover(self)(f)

    /** Behaves the same as mover when passed a monomorphic setter. May be useless. */
    def over1[T, A, B](o: Setter[S, T, A, B])(f: A => B): T = o.oover(self)(f)

    def set[T, A, B](x: B)(o: Setter[S, T, A, B]): T = o.oset(self, x)

    def view[T, A, B](o: Getter[S, T, A, B]): A = o.oview(self)

    def from[T, A, B](o: AnIso[B, A, T, S]): A = o.ofrom(self)
  }
}
