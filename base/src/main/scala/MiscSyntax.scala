package scalaFP

//TODO Get rid of this
trait MiscSyntax {

  implicit class Syntax0[S](val self: S) {
    def pure[F[_]: IApplicative]: F[S] = Applicative[F].pure(self)
    def mkConst[F[_]: IConstFunctor[S, ?[_]], A]: F[A] = ConstFunctor[S, F].mkConst(self)
  }

  implicit class Syntax1[F[_], A](self: F[A]) {
    def map[B](f: A => B)(implicit F: IFunctor[F]): F[B] = F.run.map(self)(f)
    def contramap[B](f: B => A)(implicit F: IContravariant[F]): F[B] = F.run.contramap(self)(f)
    def phantommap[B](implicit F: IPhantom[F]): F[B] = F.run.phantommap(self)
    def ap[B](f: => F[A => B])(implicit F: IApplicative[F]): F[B] = F.run.ap(self)(f)
    def bind[B](f: A => F[B])(implicit F: IMonad[F]): F[B] = F.run.bind(self)(f)
    def local[S](f: S => S)(implicit F: IMonadReader[S, F]) = F.run.local(self)(f)
    def runConst[V](implicit F: IConstFunctor[V, F]): V = F.run.runConst(self)
    def runIdentity(implicit F: IIdFunctor[F]): A = F.run.runIdentity(self)
  }

  implicit class Syntax2[P[_, _], A, B](self: P[A, B]) {
    def bimap[C, D](f: A => C, g: B => D)(implicit P: IBifunctor[P]) = P.run.bimap(self)(f, g)
    def dimap[C, D](f: C => A, g: B => D)(implicit P: IProfunctor[P]): P[C, D] = P.run.dimap(self)(f, g)
    def dualmap[C, D](f: B => D)(implicit P: IChoiceBifunctor[P]): P[C, D] = P.run.dualmap(self)(f)
    def leftMap[C](f: A => C)(implicit P: ILeft1[Functor, P]): P[C, B] = P.run.leftInstance.map(self)(f)
    def choiceleft[C](implicit P: IChoice[P]): P[Either[A, C], Either[B, C]] = P.run.left(self)
    def choiceright[C](implicit P: IChoice[P]): P[Either[C, A], Either[C, B]] = P.run.right(self)
    def toFn(implicit P: IIsFn[P]): A => B = P.run.toFn(self)
  }

  implicit class Function1Syntax[A, B](f: A => B) {
    def fromFn[P[_, _]: IIsFn]: P[A, B] = IsFn[P].fromFn(f)
  }
}
