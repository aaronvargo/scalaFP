package scalaFP

trait Review[T, B] {
  def review(b: B): T

  final def composeR[D](o: Review[B, D]): Review[T, D] = Review((review _).compose(o.review))
}

object Review {
  def apply[B, T](f: B => T): Review[T, B] = new FromReview[T, B] {
    def review(b: B): T = f(b)
  }

  trait FromReview[T, B] extends Review[T, B]
}

trait Setter[A, B, S, T] {
  def over(s: S)(f: A => B): T
  def set(s: S, b: B): T

  final def composeS[C, D](o: Setter[C, D, A, B]): Setter[C, D, S, T] =
    Setter((s, cd) => over(s)(a => o.over(a)(cd)))
}

object Setter {
  def apply[A, B, S, T](_over: (S, A => B) => T): Setter[A, B, S, T] =
    new FromOver[A, B, S, T] {
      def over(s: S)(f: A => B) = _over(s, f)
    }

  trait FromOver[A, B, S, T] extends Setter[A, B, S, T] {
    override def set(s: S, b: B): T = over(s)(_ => b)
  }

  def mapped[F[_]: Functor, A, B]: Setter[A, B, F[A], F[B]] = new FromOver[A, B, F[A], F[B]] {
    override def over(s: F[A])(f: A => B): F[B] = s.map(f)
    override def set(s: F[A], b: B): F[B] = s.mapConst(b)
  }
}

trait Traversal[A, B, S, T] extends Setter[A, B, S, T] { self =>
  def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T]

  def over(s: S)(f: A => B): T

  final def composeT[C, D](o: Traversal[C, D, A, B]): Traversal[C, D, S, T] =
    new Traversal.FromTraverse[C, D, S, T] {
      def traverse[F[_]: Applicative](s: S)(f: C => F[D]): F[T] =
        self.traverse(s)(o.traverse(_)(f))
    }
}

object Traversal {
  trait FromTraverse[A, B, S, T] extends Traversal[A, B, S, T] with Setter.FromOver[A, B, S, T] {
    override def over(s: S)(f: A => B): T =
      traverse(s)(f.andThen(Identity(_))).run
  }
}

/**
  * http://r6research.livejournal.com/28050.html
  * https://github.com/purescript-contrib/purescript-profunctor-lenses/blob/master/src/Data/Lens/Grate.purs
  *
  * Weaker than the Naperian typeclass, which supports more general versions of cotraverse and tabulate.
  */
trait Grate[A, B, S, T] extends Setter[A, B, S, T] with Review[T, B] { self =>
  def cotraverse[F[_]: Functor](s: F[S])(f: F[A] => B): T
  def tabulate(f: (S => A) => B): T

  final def composeG[C, D](o: Grate[C, D, A, B]): Grate[C, D, S, T] = new Grate.FromCotraverse[C, D, S, T] {
    def cotraverse[F[_]: Functor](s: F[S])(f: F[C] => D): T =
      self.cotraverse(s)(o.cotraverse(_)(f))
  }

  final def closed[P[_,_]: Closed](p: P[A, B]): P[S, T] = p.oclosed(this)
}

object Grate {
  def apply[A, B, S, T](_tabulate: ((S => A) => B) => T): Grate[A, B, S, T] = new FromGrate[A, B, S, T] {
    override def tabulate(f: (S => A) => B): T = _tabulate(f)
  }

  trait FromCotraverse[A, B, S, T] extends Grate[A, B, S, T] with Setter.FromOver[A, B, S, T] with Review.FromReview[T, B] {
    override def tabulate(f: (S => A) => B): T = cotraverse[S => ?](identity)(f)
    override def over(s: S)(f: A => B): T = cotraverse(Identity(s))(f.compose(_.run))
    override def review(b: B): T = cotraverse(Const[Unit, S](()))(_ => b)
  }

  trait FromGrate[A, B, S, T] extends Grate[A, B, S, T] with Setter.FromOver[A, B, S, T] with Review.FromReview[T, B] {
    override def cotraverse[F[_]: Functor](s: F[S])(f: F[A] => B): T = tabulate(g => f(s.map(g)))
    override def over(s: S)(f: A => B): T = tabulate(sa => f(sa(s)))
    override def review(b: B): T = tabulate(_ => b)
  }

  def cotraversing[F[_]: Naperian, A, B]: Grate[A, B, F[A], F[B]] = new FromCotraverse[A, B, F[A], F[B]] {
    def cotraverse[G[_]: Functor](s: G[F[A]])(f: G[A] => B): F[B] = s.cotraverse(f)
  }

  implicit def grateClosed[A, B]: Closed[Grate[A, B, ?, ?]] = {
    val x = new Closed._Oclosed[Grate[A, B, ?, ?]] {
      def oclosed[S, T, U, V](p: Grate[A, B, S, T])(g: Grate[S, T, U, V]): Grate[A, B, U, V] = g.composeG(p)
    }
    Closed.fromOclosed(x)
  }
}

trait Lens[A, B, S, T] extends Traversal[A, B, S, T] with (S => A) { self =>
  def store(s: S): (A, B => T)
  def overF[F[_]: Functor](s: S)(f: A => F[B]): F[T]

  final def composeL[C, D](o: Lens[C, D, A, B]): Lens[C, D, S, T] =
    new Lens.FromStore[C, D, S, T] {
      def store(s: S): (C, D => T) = {
        val (a, bt) = self.store(s)
        val (c, db) = o.store(a)
        (c, bt.compose(db))
      }
    }

  final def strong[P[_, _]: Strong](p: P[A, B]): P[S, T] =  p.ostrong(this)
}

object Lens {
  def apply[A, B, S, T](_get: S => A, _set: B => S => T): Lens[A, B, S, T] =
    fromStore(s => (_get(s), _set(_)(s)))

  def fromStore[A, B, S, T](f: S => (A, B => T)) = new FromStore[A, B, S, T] {
    def store(s: S) = f(s)
  }

  trait FromStore[A, B, S, T] extends Lens[A, B, S, T] with Traversal.FromTraverse[A, B, S, T] {
    override def overF[F[_]: Functor](s: S)(f: A => F[B]): F[T] = {
      val (a, bt) = store(s)
      f(a).map(bt)
    }

    override def apply(s: S): A = store(s)._1
    override def traverse[F[_]: Applicative](s: S)(f: A => F[B]) = overF(s)(f)
  }

  // def sub[U, V, A, B, S, T](l: Lens[S, T, U, V])(_get: S => A, _set: B => S => T): Lens[A, B, U, V] =
  //   l.composeL(Lens[A, B, S, T](_get, _set))

  def diamond[X]: Diamond[X] = new Diamond[X]

  class Diamond[X] {
    def apply[S, A: Has[X, ?], B: Has[X, ?]](l: Lens_[S, A], o: Setter_[S, B]): Lens_[S, A] =
      Lens(l, a => l.set(_, a).oset(a.has[X])(o.composeS(Has.lens)))
  }

  def fst[A, B, C]: Lens[A, B, (A, C), (B, C)] = Lens(_._1, x => _.copy(_1 = x))
  def snd[A, B, C]: Lens[A, B, (C, A), (C, B)] = Lens(_._2, x => _.copy(_2 = x))

  implicit def lensStrong[A, B]: Strong[Lens[A, B, ?, ?]] = {
    val x = new Strong._Ostrong[Lens[A, B, ?, ?]] {
      def ostrong[S, T, U, V](p: Lens[A, B, S, T])(l: Lens[S, T, U, V]): Lens[A, B, U, V] = l.composeL(p)
    }
    Strong.fromOstrong(x)
  }

}

trait Prism[A, B, S, T] extends Traversal[A, B, S, T] with Review[T, B] {
  def matching(s: S): Either[T, A]

  final def composeP[C, D](p: Prism[C, D, A, B]): Prism[C, D, S, T] =
    Prism(composeR(p).review, matching(_).flatMap(p.matching(_).left.map(review)))

  final def choice[P[_, _]: Choice](p: P[A, B]): P[S, T] = p.ochoice(this)
}

object Prism {
  def apply[A, B, S, T](_review: B => T, _matching: S => Either[T, A]): Prism[A, B, S, T] = new FromReviewMatching[A, B, S, T] {
    def review(b: B): T = _review(b)
    def matching(s: S): Either[T, A] = _matching(s)
  }

  trait FromReviewMatching[A, B, S, T] extends Prism[A, B, S, T] with Review.FromReview[T, B] with Traversal.FromTraverse[A, B, S, T] {
    override def traverse[F[_]: Applicative](s: S)(f: A => F[B]): F[T] =
      matching(s) match {
        case Right(a) => f(a).map(review)
        case Left(t) => t.pure[F]
      }
  }

  def left[A, B, C]: Prism[A, B, Either[A, C], Either[B, C]] = Prism(Left(_), _.map(Right(_)).swap)
  def right[A, B, C]: Prism[A, B, Either[C, A], Either[C, B]] = Prism(Right(_), _.left.map(Left(_)))

  implicit def prismChoice[A, B]: Choice[Prism[A, B, ?, ?]] = {
    val x = new Choice._Ochoice[Prism[A, B, ?, ?]] {
      def ochoice[S, T, U, V](p: Prism[A, B, S, T])(o: Prism[S, T, U, V]): Prism[A, B, U, V] =
        o.composeP(p)
    }
    Choice.fromOchoice(x)
  }
}

trait Iso[A, B, S, T] extends Lens[A, B, S, T] with Prism[A, B, S, T] with Grate[A, B, S, T] {
  final def dimapped[P[_, _]: Profunctor](p: P[A, B]): P[S, T] = p.dimap(apply, review)
  final def composeI[C, D](i: Iso[C, D, A, B]): Iso[C, D, S, T] =
    Iso(andThen(i), composeR(i).review)
}

object Iso {
  def apply[A, B, S, T](_to: S => A, _from: B => T): Iso[A, B, S, T] = new FromApplyReview[A, B, S, T] {
    override def apply(s: S): A = _to(s)
    override def review(b: B): T = _from(b)
  }

  trait FromApplyReview[A, B, S, T] extends Iso[A, B, S, T]
      with Lens.FromStore[A, B, S, T] with Prism.FromReviewMatching[A, B, S, T] with Grate.FromCotraverse[A, B, S, T] {
    override def apply(s: S): A
    override def review(b: B): T
    override def store(s: S): (A, B => T) = (apply(s), review)
    override def matching(s: S): Either[T, A] = Right(apply(s))
    override def cotraverse[F[_]: Functor](s: F[S])(f: F[A] => B): T = review(f(s.map(apply)))
  }

  implicit def isoProfunctor[A, B]: Profunctor[Iso[A, B, ?, ?]] = {
    Profunctor.fromDimap(new Profunctor._Dimap[Iso[A, B, ?, ?]] {
      override def dimap[C, D, S, T](x: Iso[A, B, C, D])(f: S => C, g: D => T): Iso[A, B, S, T] =
        Iso(f, g).composeI(x)
    })
  }
}

trait Equality[A, B, S, T] extends Iso[A, B, S, T] {
  def substBoth[P[_,_]](p: P[A, B]): P[S, T]

  final def flipEq: Equality[T, S, B, A] = {
    type P[X, Y] = Equality[Y, X, B, A]
    substBoth[P](Equality.refl)
  }

  final def subst[F[_]](x: F[A]): F[S] = {
    type P[X, Y] = F[X]
    substBoth[P](x)
  }
}

object Equality {

  trait FromSubstBoth[A, B, S, T] extends Equality[A, B, S, T] with Iso.FromApplyReview[A, B, S, T] {
    override def apply(s: S): A = {
      type P[X, Y] = X => A
      substBoth[P](identity[A])(s)
    }

    override def review(b: B): T = {
      type P[X, Y] = Y
      substBoth[P](b)
    }
  }

  private[this] object Refl extends FromSubstBoth[Any, Any, Any, Any] {
    def substBoth[P[_,_]](p: P[Any, Any]) = p
  }

  def refl[S, T]: Equality[S, T, S, T] = Refl.asInstanceOf[Equality[S, T, S, T]]
}

trait OpticModule {
  type Equality_[S, A] = Equality[A, A, S, S]
  type Lens_[S, A] = Lens[A, A, S, S]
  type Setter_[S, A] = Setter[A, A, S, S]

  implicit class OpticSyntax0_[S](self: S) {
    def oset[T, A, B](x: B)(o: Setter[A, B, S, T]): T = o.set(self, x)
    def |>[A](o: S => A): A = o(self)
  }
}
