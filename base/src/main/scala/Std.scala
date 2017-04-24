package scalaFP

trait StdModule {
  implicit class StdTupleSyntax[A, B](val self: (A, B)) {
    def cata[C](f: (A, B) => C) = f(self._1, self._2)
  }

  implicit def function1Naperian[S]: Naperian[S => ?] = {
    val x = new Naperian._Tabulate[S => ?] {
      def tabulate[A](f: Log[S => ?] => A): S => A =
        s => f(new Log[S => ?] {
                 def apply[B](g: S => B): B = g(s)
               })
    }
    Naperian.fromTabulate(x)
  }

  implicit def tuple2Functor[C]: Functor[(C, ?)] =
    Functor.fromMap(new Functor._Map[(C, ?)] {
                      def map[A, B](fa: (C, A))(f: A => B): (C, B) = (fa._1, f(fa._2))
                    })
}
