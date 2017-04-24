package scalaFP

@meta.typeclass
trait Profunctor[P[_, _]] {
  def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D]
  def leftContramap[A, B, C](p: P[A, B])(f: C => A): P[C, B]
  def rightFunctor[A]: Functor[P[A, ?]]
}

object Profunctor {
  def fromDimap[P[_,_]](dimap: _Dimap[P]): Profunctor[P] = {
    val x = new _LeftContramap[P] with _RightFunctor[P] {
      def leftContramap[A, B, C](p: P[A, B])(f: C => A): P[C, B] = dimap(p)(f, identity)
      def rightFunctor[A]: Functor[P[A, ?]] =
        Functor.fromMap(
          new Functor._Map[P[A, ?]] {
            def map[B, C](p: P[A, B])(f: B => C): P[A, C] = dimap(p)(identity, f)
          }
        )
    }
    Profunctor(dimap, x, x)
  }

  def fromContramapFunctor[P[_,_]](leftContramap: _LeftContramap[P], rightFunctor: _RightFunctor[P]) = {
    val x = new _Dimap[P] {
      def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] =
        rightFunctor.apply.map(leftContramap(p)(f))(g)
    }
  }
}

trait ProfunctorModule {
  implicit class ProfunctorSyntax[P[_,_], A, B](self: P[A, B])(implicit P: Profunctor[P]) {
    def dimap[C, D](f: C => A, g: B => D): P[C, D] = P.dimap(self)(f, g)
  }
}
