package scalaFP

trait Leibniz[A, B] {
  def subst[F[_]](fa: F[A]): F[B]
  def apply(a: A): B = subst[Lambda[a => a]](a)
  def sym: B === A = subst[? === A](Leibniz.refl)
  def cong[F[_]]: F[A] === F[B] = subst[Lambda[x => F[A] === F[x]]](Leibniz.refl)
}

object Leibniz {
  implicit def refl[A]: A === A = new (A === A) {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
}

trait LeibnizModule {
  type ===[A, B] = Leibniz[A, B]
}
