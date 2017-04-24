package scalaFP

@meta.typeclass
trait Closed[P[_,_]] {
  def oclosed[A, B, S, T](p: P[A, B])(g: Grate[A, B, S, T]): P[S, T]
  def cotraversing[A, B, F[_]: Naperian](p: P[A, B]): P[F[A], F[B]]
  val toProfunctor: Profunctor[P]
}

object Closed {
  def fromOclosed[P[_, _]](oclosed: _Oclosed[P]): Closed[P] = {
    val x = new _Cotraversing[P] with Profunctor._Dimap[P] {
      def cotraversing[A, B, F[_]: Naperian](p: P[A, B]): P[F[A], F[B]] = oclosed(p)(Grate.cotraversing)
      def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = oclosed(p)(Iso(f, g))
    }
    Closed(oclosed, x, Profunctor.fromDimap(x))
  }

  /** Potentially very inefficient since it uses tabulate */
  def fromCotraversingProfunctor[P[_,_]](cotraversing: _Cotraversing[P], profunctor: Profunctor[P]): Closed[P] = {
    val x = new _Oclosed[P] {
      def oclosed[A, B, S, T](p: P[A, B])(g: Grate[A, B, S, T]): P[S, T] =
        profunctor.dimap(cotraversing[A, B, (S => A) => ?](p))(s => sa => sa(s), g.tabulate)
    }
    Closed(x, cotraversing, profunctor)
  }
}

trait ClosedModule {
  implicit class ClosedSyntax[P[_, _], A, B](val self: P[A, B])(implicit P: Closed[P]) {
    def oclosed[S, T](g: Grate[A, B, S, T]): P[S, T] = P.oclosed(self)(g)
    def cotraversing[F[_]: Naperian]: P[F[A], F[B]] = P.cotraversing(self)
  }
}
