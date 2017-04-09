package scalaFP

trait Apply[F[_]] extends Functor[F] with Apply.Ap[F] with Apply.ApFst[F] with Apply.ApSnd[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B]
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  override def apFst[A, B](fa: F[A], fb: F[B]): F[A] = ap(fa)(mapConst(fb, identity[A]))
  override def apSnd[A, B](fa: F[A], fb: F[B]): F[B] = ap(fa)(map(fb)(b => _ => b))
}

object Apply {
  def apply[F[_]](implicit e: IApply[F]): Apply[F] = e.run

  trait Ap[F[_]] {
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  }

  trait ApFst[F[_]] {
    def apFst[A, B](fa: F[A], fb: F[B]): F[A]
  }

  trait ApSnd[F[_]] {
    def apSnd[A, B](fa: F[A], fb: F[B]): F[B]
  }

  trait From[F[_]] extends Apply[F] with Functor.From[F] {
    def applyDelegate: Apply[F]
    override def functorDelegate: Functor[F] = applyDelegate
    override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = applyDelegate.ap(fa)(f)
    override def apFst[A, B](fa: F[A], fb: F[B]): F[A] = applyDelegate.apFst(fa, fb)
    override def apSnd[A, B](fa: F[A], fb: F[B]): F[B] = applyDelegate.apSnd(fa, fb)
  }

  def construct[F[_]](_ap: Ap[F],
                      _apFst: ApFst[F],
                      _apSnd: ApSnd[F],
                      _functor: Functor[F]): Apply[F] =
    new Apply[F] with Functor.From[F] {
      def functorDelegate = _functor
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = _ap.ap(fa)(f)
      override def apFst[A, B](fa: F[A], fb: F[B]): F[A] = _apFst.apFst(fa, fb)
      override def apSnd[A, B](fa: F[A], fb: F[B]): F[B] = _apSnd.apSnd(fa, fb)
    }

  def copy[F[_]](x: Apply[F])(_ap : Ap[F] = x,
                              _apFst: ApFst[F] = x,
                              _apSnd: ApSnd[F] = x,
                              _functor: Functor[F] = x): Apply[F] =
    construct(_ap, _apFst, _apSnd, _functor)

  def apL[F[_]]: Lens_[Apply[F], Ap[F]] = Optic.superLens(x => copy(_)(_ap = x))
  def apFstL[F[_]]: Lens_[Apply[F], ApFst[F]] = Optic.superLens(x => copy(_)(_apFst = x))
  def apSndL[F[_]]: Lens_[Apply[F], ApSnd[F]] = Optic.superLens(x => copy(_)(_apSnd = x))
  def functorL[F[_]]: Lens_[Apply[F], Functor[F]] = Optic.superLens(x => copy(_)(_functor = x))

  def mapL[F[_]]: Lens_[Apply[F], Functor.Map[F]] = functorL.compose(Functor.mapL)

  def fromMapAp[F[_]](_map: Functor.Map[F], _ap: Ap[F]): Apply[F] = {
    new Apply[F] with Functor.From[F] {
      def functorDelegate = Functor.fromMap(_map)
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = _ap.ap(fa)(f)
    }
  }
}
