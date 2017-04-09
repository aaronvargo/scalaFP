package scalaFP

trait Contrapplicative[F[_]] extends Phantom[F] with Applicative[F]

object Contrapplicative {
  def apply[F[_]](implicit e: IContrapplicative[F]) = e.run

  trait From[F[_]] extends Phantom.From[F] with Applicative.From[F] {
    def contrapplicativeDelegate: Contrapplicative[F]
    override def phantomDelegate: Phantom[F] = contrapplicativeDelegate
    override def applicativeDelegate: Applicative[F] = contrapplicativeDelegate
  }
}
