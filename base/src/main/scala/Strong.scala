package scalaFP

@meta.typeclass
trait Strong[P[_, _]] {
  def ostrong[A, B, S, T](p: P[A, B])(l: Lens[A, B, S, T]): P[S, T]
  def strong2[A, B, C](p: P[A, B]): P[(C, A), (C, B)]
  val toProfunctor: Profunctor[P]
}

object Strong {

  def fromOstrong[P[_,_]](ostrong: _Ostrong[P]): Strong[P] = {
    val x = new Profunctor._Dimap[P] with _Strong2[P] {
      override def dimap[A, B, C, D](p: P[A, B])(f: C => A, g: B => D): P[C, D] =
        ostrong(p)(Iso(f, g))
      override def strong2[A, B, C](p: P[A, B]): P[(C, A), (C, B)] = ostrong(p)(Lens.snd)
    }
    Strong(ostrong, x, Profunctor.fromDimap(x))
  }

  def fromStrong2Profunctor[P[_,_]](strong2: _Strong2[P], profunctor: Profunctor[P]): Strong[P] = {
    val x = new _Ostrong[P] {
      override def ostrong[A, B, S, T](p: P[A, B])(l: Lens[A, B, S, T]): P[S, T] =
        profunctor.dimap(strong2[A, B, B => T](p))(l.store(_).swap, _.cata(_ apply _))
    }
    Strong(x, strong2, profunctor)
  }
}

trait StrongModule {
  implicit class StrongSyntax[P[_,_], A, B](val self: P[A, B])(implicit P: Strong[P]) {
    def ostrong[S, T](l: Lens[A, B, S, T]): P[S, T] = P.ostrong(self)(l)
  }
}
