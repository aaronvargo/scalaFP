package scalaFP

trait Bind[F[_]] extends Apply[F] with Bind.MBind[F] with Bind.Join[F] {
  def join[A, B](fa: F[F[A]]): F[A] = bind(fa)(identity)
}

object Bind {
  def apply[F[_]](implicit e: IBind[F]): Bind[F] = e.run

  trait MBind[F[_]] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Join[F[_]] {
    def join[A, B](fa: F[F[A]]): F[A]
  }

  trait From[F[_]] extends Bind[F] with Apply.From[F] {
    def bindDelegate: Bind[F]
    def applyDelegate = bindDelegate
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = bindDelegate.bind(fa)(f)
    override def join[A, B](fa: F[F[A]]): F[A] = bindDelegate.join(fa)
  }

  trait FromBindMap[F[_]] extends Bind[F] {
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = bind(f)(map(fa))
  }

  trait FromJoinMap[F[_]] extends Bind[F] with FromBindMap[F] {
    override def join[A, B](fa: F[F[A]]): F[A]
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
  }
}
