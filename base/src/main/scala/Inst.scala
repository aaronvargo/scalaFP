package scalaFP

trait InstNewtype {
  type Inst[A]
  def apply[A](a: A): Inst[A]
  def run[A](implicit i: Inst[A]): A
  def coerce[A, B]: Equality[Inst[A], Inst[B], A, B]
  def widen[A, B >: A](i: Inst[A]): Inst[B]
}

object InstAlias extends InstNewtype {
  type Inst[A] = A
  def apply[A](a: A): A = a
  def run[A](implicit i: A): A = i
  def coerce[A, B]: Equality[A, B, A, B] = Optic.id
  def widen[A, B >: A](i: A): B = i
  val Newtype: InstNewtype = this
}

trait InstModule  {
  val Inst: InstAlias.Newtype.type = InstAlias.Newtype
  type Inst[A] = Inst.Inst[A]

  implicit class InstSyntax[A](self: Inst[A]) {
    def run: A = Inst.run(self)
    def widen[B >: A]: Inst[B] = Inst.widen(self)
  }
}

