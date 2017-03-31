package scalaFP

trait ConstNewtype {
  type Const[A, B]
  def apply[A, B](a: A): Const[A, B]
  def run[A, B](a: Const[A, B]): A
  def coerce[S, T, A, B]: Equality[Const[S, A], Const[T, B], S, T]
  def constFunctor[V]: IConstFunctor[V, Const[V, ?]]
}

object ConstAlias extends ConstNewtype {
  type Const[A, B] = A
  def apply[A, B](a: A): A = a
  def run[A, B](a: A): A = a
  def coerce[S, T, A, B]: Equality[S, T, S, T] = Optic.id
  def constFunctor[V]: IConstFunctor[V, Const[V, ?]] =
    Inst(new ConstFunctor.FromRunMk[V, Const[V, ?]] {
           def mkConst[A](v: V): V = v
           def runConst[A](x: V): V = x
         })
  val Newtype: ConstNewtype = this
}

trait ConstModule {
  val Const: ConstAlias.Newtype.type = ConstAlias.Newtype
  type Const[A, B] = Const.Const[A, B]

  implicit def constConstFunctor[V]: IConstFunctor[V, Const[V, ?]] = Const.constFunctor

  implicit class ConstSyntax[A, B](self: Const[A, B]) {
    def run: A = Const.run(self)
  }
}
