package scalaFP

@meta.typeclass
trait Choice[P[_, _]] {
  def ochoice[A, B, S, T](p: P[A, B])(o: Prism[A, B, S, T]): P[S, T]
  def choiceR[A, B, C](p: P[A, B]): P[Either[C, A], Either[C, B]]
  val toProfunctor: Profunctor[P]
}

object Choice {
  def fromOchoice[P[_,_]](ochoice: _Ochoice[P]): Choice[P] = {
    val x = new Profunctor._Dimap[P] with _ChoiceR[P] {
      def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] = ochoice(p)(Iso(f, g))
      def choiceR[A, B, C](p: P[A,B]): P[Either[C,A],Either[C,B]] = ochoice(p)(Prism.right)
    }
    Choice(ochoice, x, Profunctor.fromDimap(x))
  }
}

trait ChoiceModule {
  implicit class ChoiceSyntax[P[_,_], A, B](val self: P[A, B])(implicit P: Choice[P]) {
    def ochoice[S, T](o: Prism[A, B, S, T]): P[S, T] = P.ochoice(self)(o)
  }
}
