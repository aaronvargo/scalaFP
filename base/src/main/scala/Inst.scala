package scalaFP

trait InstNewtype {
  type Inst[A]
  def coerce[A]: A === Inst[A]
  def apply[A](a: A): Inst[A] = coerce(a)
  def run[A](implicit i: Inst[A]): A = coerce.sym(i)
  def widen[A, B >: A](i: Inst[A]): Inst[B] = apply(run(i))
}

object InstAlias extends InstNewtype {
  type Inst[A] = A
  def coerce[A]: A === A = Leibniz.refl
  val Newtype: InstNewtype = this
}

trait InstModule  {
  val Inst: InstAlias.Newtype.type = InstAlias.Newtype
  type Inst[A] = Inst.Inst[A]
  implicit def instSyntax[A](i: Inst[A]): InstSyntax[A] = new InstSyntax[A](i)
}

class InstSyntax[A](val self: Inst[A]) extends AnyVal {
  def widen[B >: A]: Inst[B] = Inst.widen(self)
  def run: A = Inst.run(self)
}
