package scalaFP

trait IdentityType {
  type Identity[A]
  def coerce[A]: A === Identity[A]
  def apply[A](a: A): Identity[A] = coerce(a)
  def run[A](x: Identity[A]): A = coerce.sym(x)
  def idFunctor: IIdFunctor[Identity] =
    Inst(new IdFunctor.FromRunPure[Identity] {
           def pure[A](a: A): Identity[A] = apply(a)
           def runIdentity[A](fa: Identity[A]): A = run(fa)
         })
}

object IdentityAlias extends IdentityType {
  type Identity[A] = A
  def coerce[A]: A === A = Leibniz.refl
  val Newtype: IdentityType = this
}

trait IdentityModule {
  val Identity: IdentityAlias.Newtype.type = IdentityAlias.Newtype
  type Identity[A] = Identity.Identity[A]

  implicit class IdentitySyntax[A](self: Identity[A]) {
    def run: A = Identity.run(self)
  }

  implicit val identityIdFunctor: IIdFunctor[Identity] = Identity.idFunctor
}
