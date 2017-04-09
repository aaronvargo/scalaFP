package scalaFP

trait Choice[P[_, _]] extends Profunctor[P] {
  def left[A, B, C](fa: P[A, B]): P[Either[A, C], Either[B, C]]
  def right[A, B, C](fa: P[A, B]): P[Either[C, A], Either[C, B]]
}

object Choice {
  def apply[P[_, _]](implicit e: IChoice[P]) = e.run

  trait From[P[_, _]] extends Choice[P] with Profunctor.From[P] {
    def choiceDelegate: Choice[P]
    override def profunctorDelegate: Profunctor[P] = choiceDelegate
    override def left[A, B, C](fa: P[A, B]): P[Either[A, C], Either[B, C]] = choiceDelegate.left(fa)
    override def right[A, B, C](fa: P[A, B]): P[Either[C, A], Either[C, B]] = choiceDelegate.right(fa)
  }
}
