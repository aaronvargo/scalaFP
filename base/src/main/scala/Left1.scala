package scalaFP

trait Left1[+C[_[_]], P[_, _]] {
  def leftInstance[A]: C[P[?, A]]
}

object Left1 {
  def apply[C[_[_]], P[_, _]](implicit e: ILeft1[C, P]) = e.run
}
