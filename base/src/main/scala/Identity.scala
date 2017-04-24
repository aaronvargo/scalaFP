package scalaFP

@meta.newtype
object IdentityType {

  type Identity[A] = A

  val monad: Monad[Identity] = {
    val x = new Bind._Bind[Identity] with Applicative._Pure[Identity] {
      override def bind[A, B](a: A)(f: A => B): B = f(a)
      override def pure[A](a: A): A = a
    }
    Monad.fromBindPure(x, x)
  }
}

trait IdentityModule extends IdentityType.TopLevel {
  type Id[A] = IdentityAlias[A]
  implicit val identityMonad: Monad[Identity] = Identity.monad
}

