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

  def compose[PC[_[_, _]], FC[_[_]], S, T, A, B, C, D]
    (o1: Optic[PC, FC, S, T, A, B], o2: Optic[PC, FC, A, B, C, D])
      : Optic[PC, FC, S, T, C, D] =
    new _Optic[PC, FC, S, T, C, D] {
      def transform[P[_, _], F[_]](f: P[C, F[D]])
                   (implicit P: Inst[PC[P]], F: Inst[FC[F]]): P[S, F[T]] =
        o1.transform[P, F](o2.transform[P, F](f))
    }.optic

  private[this] object IdOptic extends Optic[Any2, Any1, Any, Any, Any, Any] {
    def explicitTransform[P[_, _], F[_]]
      (f: P[Any, F[Any]], P: Any2[P], F: Any1[F]): P[Any, F[Any]] = f
  }

  def id[PC[_[_, _]], FC[_[_]], S, T]: Optic[PC, FC, S, T, S, T] =
    IdOptic.asInstanceOf[Optic[PC, FC, S, T, S, T]]

  def iso[S, T, A, B](f: S => A, g: B => T): Iso[S, T, A, B] =
    new _Optic[Profunctor, Functor, S, T, A, B] {
      def transform[P[_, _]: IProfunctor, F[_]: IFunctor](p: P[A, F[B]]): P[S, F[T]] = p.dimap(f, _.map(g))
    }.optic

  def review[S, T, A, B](_review: B => T): Review[S, T, A, B] = new _Optic[ChoiceBifunctor, IdFunctor, S, T, A, B] {
    def transform[P[_, _] : IChoiceBifunctor, F[_] : IIdFunctor](p: P[A,F[B]]): P[S,F[T]] =
      p.dualmap(_.map(_review))
  }.optic

  def prism[S, T, A, B](_review: B => T, _matching: S => Either[T, A]): Prism[S, T, A, B] =
    new _Optic[Choice, Applicative, S, T, A, B] {
      def transform[P[_,_] : IChoice, F[_]: IApplicative](p: P[A, F[B]]): P[S, F[T]] =
        p.choiceright.dimap(_matching, (_ : Either[T, F[B]]) match {
                              case Left(x) => x.pure[F]
                              case Right(x) => x.map(_review)
                            })
    }.optic

  def getter[S, T, A, B](_get: S => A): Optic[Profunctor, Phantom, S, T, A, B] =
    new _Optic[Profunctor, Phantom, S, T, A, B] {
      def transform[P[_, _]: IProfunctor, F[_]: IPhantom](f: P[A, F[B]]): P[S, F[T]] =
        f.dimap(_get, _.phantommap)
    }.optic

  def setter[S, T, A, B](_modify: (A => B, S) => T): Setter[S, T, A, B] =
    new _LensLike[IdFunctor, S, T, A, B] {
      def overF[F[_]: IIdFunctor](f: A => F[B])(s: S): F[T] =
        _modify(f.andThen(_.runIdentity), s).pure[F]
    }.optic

  def lens[S, T, A, B](_get: S => A, _set: B => S => T): Lens[S, T, A, B] =
    new _LensLike[Functor, S, T, A, B] {
      def overF[F[_]: IFunctor](f: A => F[B])(s: S): F[T] = f(_get(s)).map(_set(_)(s))
    }.optic

  /** Construct a lens to a supertype */
  def superLens[S, T, A >: S, B](_set: B => S => T): Lens[S, T, A, B] = lens(identity, _set)

  implicit class OpticSyntax[PC[_[_, _]], FC[_[_]], S, T, A, B](self: Optic[PC, FC, S, T, A, B]) {
    //Can't be added directly to the class due to variance
    def transform[P[_, _], F[_]](f: P[A, F[B]])
                 (implicit P: Inst[PC[P]], F: Inst[FC[F]]): P[S, F[T]] =
      self.explicitTransform[P, F](f, P.run, F.run)

    /** Assumes second argument of P has representational role */
    def transformId[P[_,_]](p: P[A, B])(implicit P: Inst[PC[P]], e: Inst[FC[Identity]]): P[S, T] =
      Identity.coerce.flipEq.subst[P[S, ?]](self.transform[P, Identity](Identity.coerce.subst[P[A, ?]](p)))

    /** Assumes second argument of P has representational role */
    def explicitTransformId[P[_,_]](p: P[A, B], P: PC[P])(implicit e: Inst[FC[Identity]]): P[S, T] = transformId(p)(Inst(P), e)
  }

  implicit class LensLikeSyntax[C[_[_]], S, T, A, B](self: LensLike[C, S, T, A, B]) {
    def overF[F[_]](s: S, f: A => F[B])(implicit F: Inst[C[F]]): F[T] = self.transform[Function1, F](f).apply(s)
  }

  implicit class EqualitySyntax[S, T, A, B](self: Equality[S, T, A, B]) {
    def subst[F[_]](x: F[A]): F[S] = {
      type P[X, Y] = F[X]
      self.transform[P, F](x)
    }

    def flipEq: Equality[B, A, T, S] = {
      type P[X, Y] = Equality[B, A, Y, X]
      self.transform[P, Id](Optic.id)
    }
  }

  implicit class IsoSyntax[S, T, A, B](self: Iso[S, T, A, B]) {
    def toIso: Iso[S, T, A, B] = self

    def isoPair: (S => A, B => T) = {
      type P[X, Y] = (X => A, B => Y)
      def pProfunctor: Profunctor[P] = new Profunctor[P] {
        def dimap[A0, B0, C, D](p: (A0 => A, B => B0))(f: C => A0, g: B0 => D): (C => A, B => D) =
          p.bimap(_.compose(f), _.andThen(g))
      }
      self.explicitTransformId[P]((identity, identity), pProfunctor)
    }

    def foldIso[R](f: (S => A, B => T) => R): R = f.tupled(isoPair)

    def flipIso: Iso[B, A, T, S] = foldIso((f, g) => Optic.iso(g, f))
  }

  implicit class PrismSyntax[S, T, A, B](self: Prism[S, T, A, B]) {
    def toPrism: Prism[S, T, A, B] = self

    def prismPair: (B => T, S => Either[T, A]) = {
      type P[X, Y] = (B => Y, X => Either[Y, A])
      def pChoice = new Choice[P] {
        def dimap[A0, B0, C, D](p: (B => B0, A0 => Either[B0, A]))(f: C => A0, g: B0 => D): (B => D, C => Either[D, A]) =
          p.bimap(_.andThen(g), _.dimap(f, _.leftMap(g)))
        def left[A0, B0, C](p: (B => B0, A0 => Either[B0, A])): (B => Either[B0,C], Either[A0,C] => Either[Either[B0, C], A]) =
          p.bimap(_.andThen(_.left), f => _.leftMap(f) match {
                    case Right(a) => Left(Right(a))
                    case Left(Right(c)) => Right(c)
                    case Left(Left(b)) => Left(Left(b))
                  })
        def right[A0, B0, C](p: (B => B0, A0 => Either[B0, A])): (B => Either[C, B0], Either[C, A0] => Either[Either[C, B0], A]) =
          p.bimap(_.andThen(_.right), f => _.map(f) match {
                    case Left(c) => Left(Left(c))
                    case Right(Left(b)) => Left(Right(b))
                    case Right(Right(a)) => Right(a)
                  })
      }
      self.explicitTransformId[P]((identity, _.right), pChoice)
    }

    def matching(s: S): Either[T, A] = prismPair._2(s)
  }

  implicit class ReviewSyntax[S, T, A, B](self: Review[S, T, A, B]) {
    def toReview: Review[S, T, A, B] = self

    def review(b: B): T = {
      type P[X, Y] = Y
      def pChoiceBifunctor = new ChoiceBifunctor[P] {
        def dualmap[A0, B0, C0, D0](p: B0)(f: B0 => D0): D0 = f(p)
      }
      self.explicitTransformId[P](b, pChoiceBifunctor)
    }
  }

  implicit class SetterSyntax[S, T, A, B](self: Setter[S, T, A, B]) {
    def toSetter: Setter[S, T, A, B] = self
    def over(s: S)(f: A => B): T = self.overF(s, f.andThen(Identity(_))).run
    def set(s: S, x: B): T = over(s)(_ => x)
  }

  implicit class GetterSyntax[S, T, A, B](self: Getter[S, T, A, B]) {
    def toGetter: Getter[S, T, A, B] = self
    def view(s: S): A = self.overF[Const[A, ?]](s, Const(_)).runConst
  }

  implicit class GetterLikeSyntax[C[F[_]] <: Phantom[F], S, T, A, B](self: LensLike[C, S, T, A, B]) {
    def getting: LensLike_[C, S, A] = new _LensLike[C, S, S, A, A] {
      def overF[F[_]](f: A => F[A])(s: S)(implicit F: Inst[C[F]]): F[S] = {
        implicit val phantomF: IPhantom[F] = F.widen
        self.overF[F](s, f.andThen(_.phantommap)).phantommap
      }
    }.optic
  }

  implicit class FoldSyntax[S, T, A, B](self: Fold[S, T, A, B]) {
    def toFold: Fold[S, T, A, B] = self
  }

  implicit class TraversalSyntax[S, T, A, B](self: Traversal[S, T, A, B]) {
    def toTraversal: Traversal[S, T, A, B] = self
  }

  implicit class LensSyntax[S, T, A, B](self: Lens[S, T, A, B]) {
    def toLens: Lens[S, T, A, B] = self
  }

  def fst[A, B, C]: Lens[(A, C), (B, C), A, B] = lens(_._1, x => _.copy(_1 = x))
  def snd[A, B, C]: Lens[(A, B), (A, C), B, C] = lens(_._2, x => _.copy(_2 = x))
}

trait OpticModule {
  type Equality[S, T, A, B] = Optic[Any2, Any1, S, T, A, B]
  type Iso[S, T, A, B] = Optic[Profunctor, Functor, S, T, A, B]
  type Prism[S, T, A, B] = Optic[Choice, Applicative, S, T, A, B]
  type Review[S, T, A, B] = Optic[ChoiceBifunctor, IdFunctor, S, T, A, B]
  type LensLike[-C[_[_]], S, T, A, B] = Optic[IsFn, C, S, T, A, B]
  type Lens[S, T, A, B] = LensLike[Functor, S, T, A, B]
  type Traversal[S, T, A, B] = LensLike[Applicative, S, T, A, B]
  type Getter[S, T, A, B] = LensLike[Phantom, S, T, A, B]
  type Getting[R, S, T, A, B] = LensLike[ConstFunctor[R, ?[_]], S, T, A, B]
  type Fold[S, T, A, B] = LensLike[Contrapplicative, S, T, A, B]
  type Setter[S, T, A, B] = LensLike[IdFunctor, S, T, A, B]

  type Optic_[PC[_[_, _]], FC[_[_]], S, A] = Optic[PC, FC, S, S, A, A]
  type Equality_[S, A] = Equality[S, S, A, A]
  type Iso_[S, A] = Iso[S, S, A, A]
  type Prism_[S, A] = Prism[S, S, A, A]
  type Review_[S, A] = Review[S, S, A, A]
  type LensLike_[-C[_[_]], S, A] = LensLike[C, S, S, A, A]
  type Lens_[S, A] = Lens[S, S, A, A]
  type Getting_[R, S, A] = Getting[R, S, S, A, A]
  type Getter_[S, A] = Getter[S, S, A, A]
  type Setter_[S, A] = Setter[S, S, A, A]
  type Fold_[S, A] = Fold[S, S, A, A]
  type Traversal_[S, A] = Traversal[S, S, A, A]

  implicit class OpticSyntax0_[S](self: S) {
    def ooverF[C[_[_]], F[_], T, A, B](f: A => F[B])(o: LensLike[C, S, T, A, B])(implicit F: Inst[C[F]]): F[T] = o.overF(self, f)

    def moverF[C[_[_]], F[_], A](f: A => F[A])(o: LensLike_[C, S, A])(implicit F: Inst[C[F]]): F[S] = o.overF(self, f)

    /** Polymorphic over. Can't infer the argument type of the function. */
    def oover[T, A, B](f: A => B)(o: Setter[S, T, A, B]): T = o.over(self)(f)

    def oover1[T, A, B](o: Setter[S, T, A, B])(f: A => B): T = o.over(self)(f)

    /** Monomorphic over. Has better inference; can infer A. */
    def mover[T, A, B](o: Setter_[S, A])(f: A => A): S = o.over(self)(f)

    def oset[T, A, B](x: B)(o: Setter[S, T, A, B]): T = o.set(self, x)

    def oview[T, A, B](o: Getter[S, T, A, B]): A = o.view(self)
  }
}
