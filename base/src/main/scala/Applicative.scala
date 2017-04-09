package scalaFP

trait Applicative[F[_]] extends Apply[F] {
  def pure[A](a: A): F[A]
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
}

object Applicative {
  def apply[F[_]](implicit e: IApplicative[F]) = e.run

  trait From[F[_]] extends Applicative[F] with Apply.From[F] {
    def applicativeDelegate: Applicative[F]
    override def applyDelegate: Apply[F] = applicativeDelegate
    override def pure[A](a: A): F[A] = applicativeDelegate.pure(a)
    override def map[A, B](fa: F[A])(f: A => B): F[B] = applicativeDelegate.map(fa)(f)
  }
}
