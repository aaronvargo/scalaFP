package scalaFP

trait ConstNewtype {
  type Const[A, B]
  def coerce[A, B]: A === Const[A, B]
  def apply[A, B](a: A): Const[A, B] = coerce(a)
  def run[A, B](x: Const[A, B]) = coerce.sym(x)
  def constFunctor[V]: IConstFunctor[V, Const[V, ?]] =
    Inst(new ConstFunctor.FromRunMk[V, Const[V, ?]] {
           def mkConst[A](v: V): Const[V, A] = apply(v)
           def runConst[A](x: Const[V, A]): V = run(x)
         })
}

object ConstAlias extends ConstNewtype {
  type Const[A, B] = A
  def coerce[A, B]: A === A = Leibniz.refl
  val Newtype: ConstNewtype = this
}

trait ConstModule {
  val Const: ConstAlias.Newtype.type = ConstAlias.Newtype
  type Const[A, B] = Const.Const[A, B]
  implicit def constConstFunctor[V]: IConstFunctor[V, Const[V, ?]] = Const.constFunctor
}
