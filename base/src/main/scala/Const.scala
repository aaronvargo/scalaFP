package scalaFP

@meta.newtype
object ConstType {
  type Const[A, B] = A
  def functor[S]: Functor[Const[S, ?]] = {
    val x = new Functor._Map[Const[S, ?]] {
      def map[A, B](fa: Const[S, A])(f: A => B): Const[S, B] = fa
    }
    Functor.fromMap[Const[S, ?]](x)
  }
}

trait ConstModule extends ConstType.TopLevel {
  implicit def constFunctor[S]: Functor[Const[S, ?]] = Const.functor[S]
}
