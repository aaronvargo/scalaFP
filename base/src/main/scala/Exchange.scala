package scalaFP

case class Exchange[A, B, S, T](to: S => A, from: B => T)

trait ExchangeModule {
  implicit def exchangeProfunctor[A, B]: IProfunctor[Exchange[A, B, ?, ?]] =
    Inst(new Profunctor[Exchange[A, B, ?, ?]] {
           def dimap[S, T, C, D](p: Exchange[A, B, S, T])(f: C => S, g: T => D): Exchange[A, B, C, D] =
             Exchange(p.to.compose(f), p.from.andThen(g))
         })
}
