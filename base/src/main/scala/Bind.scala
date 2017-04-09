package scalaFP

trait Bind[F[_]] extends Apply[F] {
  override def map[A, B](fa: F[A])(f: A => B): F[B]
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A, B](fa: F[F[A]]): F[A] = bind(fa)(identity)
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(map(fa))
}

object Bind {
  def apply[F[_]](implicit e: IBind[F]): Bind[F] = e.run

  trait From[F[_]] extends Bind[F] with Apply.From[F] {
    def bindDelegate: Bind[F]
    override def applyDelegate: Apply[F] = bindDelegate
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = bindDelegate.bind(fa)(f)
    override def join[A, B](fa: F[F[A]]): F[A] = bindDelegate.join(fa)
    override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = bindDelegate.ap(fa)(f)
  }

  trait FromJoin[F[_]] extends Bind[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B]
    override def join[A, B](fa: F[F[A]]): F[A]
    override def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  }
}
