package scalaFP

trait IdentityType {
  type Identity[A]
  def apply[A](a: A): Identity[A]
  def run[A](a: Identity[A]): A
  def coerce[A, B]: Equality[Identity[A], Identity[B], A, B]
  def idFunctor: IIdFunctor[Identity]
}

object IdentityAlias extends IdentityType {
  type Identity[A] = A
  def apply[A](a: A): A = a
  def run[A](a: A): A = a
  def coerce[A, B]: Equality[A, B, A, B] = Optic.id
  val idFunctor: IIdFunctor[Identity] =
    Inst(new IdFunctor.FromRunPure[Identity] {
           def pure[A](a: A): A = a
           def runIdentity[A](fa: A): A = fa
         })
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
