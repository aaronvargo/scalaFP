package scalaFP

trait Right1[+C[_[_]], P[_, _]] {
  def rightInstance[A]: C[P[A, ?]]
}

object Right1 {
  def apply[C[_[_]], P[_, _]](implicit e: IRight1[C, P]) = e.run
}
