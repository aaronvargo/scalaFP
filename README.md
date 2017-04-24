An experimental modern FP library in Scala, currently featuring:

- subtyping-and-inheritance-free typeclasses, defined as case classes where
  members can be overridden using lenses.
- coercible newtypes, based on the tagged-type trick
- macros to generate boilerplate associated with the above two features
- optics. The design will probably be changed later, to not use inheritance.

Additional minor things:
- `Profunctor[P]` implies `Functor[P[A, ?]]`, forall A
- `Naperian` (A stronger version of `Distributive`, equivalent to `Representable`) functors and logarithm types

# Typeclass Encoding

The `Bind` class is defined as follows:

~~~scala
@meta.typeclass
trait Bind[F[_]] {
  def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  def join[A](ffa: F[F[A]]): F[A]
  val toApply: Apply[F]
}
~~~

The `typeclass` macro combines the `caseclassy` and `classyLenses` macros
together, and generates the following:

~~~scala
case class Bind[F[_]](bind: Bind._Bind[F], join: Bind._Join[F], toApply: Apply[F])

object Bind {

  trait _Bind[F[_]] {
    def bind[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  object _Bind {
    implicit class Syntax[F[_]](val self: _Bind[F]) {
      def apply[A, B](fa: F[A])(f: A => F[B]): F[B] = self.bind[A, B](fa)(f)
    }
  }

  trait _Join[F[_]] {
    def join[A](ffa: F[F[A]]): F[A]
  }

  object _Join {
    implicit class Syntax[F[_]](val self: _Join[F]) {
      def apply[A](ffa: F[F[A]]): F[A] = self.join[A](ffa)
    }
  }

  def _bind[F[_]]: Lens_[Bind[F], _Bind[F]] = Lens(_.bind, x => _.copy(bind = x))
  def _join[F[_]]: Lens_[Bind[F], _Join[F]] = Lens(_.join, x => _.copy(join = x))
  def _toApply[F[_]]: Lens_[Bind[F], Apply[F]] = Lens(_.toApply, x => _.copy(toApply = x))

  def lens[F[_], T: Has[Bind[F], ?]]: Lens_[T, Bind[F]] = Has.lens 

  def bind[F[_], T: Has[Bind[F], ?]]: Lens_[T, Bind._Bind[F]] = lens.composeL(_bind) 
  def join[F[_], T: Has[Bind[F], ?]]: Lens_[T, Bind._Join[F]] = lens.composeL(_join) 
  def toApply[F[_], T: Has[Bind[F], ?]]: Lens_[T, Apply[F]] = lens.composeL(_toApply) 
}
~~~

The first thing to note is that it generates a trait for each def. This allows
lenses to be defined for each method. It then converts the typeclass to a case
class. 

An apply method is also added as syntax to each method-trait. Giving the
original methods different names, and only adding apply as syntax, allows the
traits to be mixed together for convenience, as shown later.

The next thing to note is that it generates both non-classy and classy lenses
for each field. The non-classy lenses should probably be removed, but I haven't
done that yet. The classy lenses make use of the `Has` typeclass, and will work
with any "subclass" of the typeclass. Their primary purpose is to allow
overriding of methods, via set.

## Typeclass Hierarchy and the Has Typeclass

To make `Bind` a "subclass" of `Apply`, the following implicits also need to be
added to the [scato](https://github.com/aloiscochard/scato)-style hierarchy:

~~~scala
implicit def bindApply[F[_]](implicit e: Bind[F]): Apply[F] = e.toApply
implicit def hasBindApply[F[_], A](implicit e: Has[Bind[F], A]): Has[Apply[F], A] = e.upcast(Bind.toApply)
~~~

The first produces an implicit `Apply` whenever there's an implicit `Bind`. The
second witnesses that anything which `Has` a `Bind` also `Has` an `Apply`. This
allows the classy lenses for `Apply` to be used with `Bind`.

## Construction of Typeclass Instances:

Typeclasses are constructed via regular functions:

~~~scala
object Bind {
  def fromBindMap[F[_]](bind: _Bind[F], map: Functor._Map[F]): Bind[F] = {
    val x = new Apply._Ap[F] with _Join[F] {
      def ap[A, B](fa: F[A])(fab: F[A => B]): F[B] = bind(fab)(map(fa))
      def join[A](ffa: F[F[A]]): F[A] = bind(ffa)(identity)
    }
    Bind(bind, x, Apply.fromApMap(x, map))
  }

  def fromJoinMap[F[_]](join: _Join[F], map: Functor._Map[F]): Bind[F] = {
    val x = new _Bind[F] {
      def bind[A, B](fa: F[A])(f: A => F[B]): F[B] = join(map(fa)(f))
    }
    fromBindMap(x, map).oset(join)(Bind.join)
  }
}
~~~

Note how the first definition mixes method-traits together for convenience, and
how the second uses a lens to override the join method.

# Coercible Newtypes

The `Identity` newtype is defined as:

~~~scala
@meta.newtype
object IdentityType {

  type Identity[A] = A

  val monad: Monad[Identity] = {
    val x = new Bind._Bind[Identity] with Applicative._Pure[Identity] {
      override def bind[A, B](a: A)(f: A => B): B = f(a)
      override def pure[A](a: A): A = a
    }
    Monad.fromBindPure(x, x)
  }
}
~~~

The `newtype` macro transforms the above into:

~~~scala
object IdentityType {

  trait Module {
    type Identity[A]
    val monad: Monad[Identity]
    def apply[A](a: A): Identity[A]
    def run[A](a: Identity[A]): A
    def coercion[A, $A0]: Equality[A, Alias.Identity[$A0], Identity[A], Identity[$A0]]
  }

  object Alias extends Module {
    type Identity[A] = A
    val monad: Monad[Identity] = {
      val x = new Bind._Bind[Identity] with Applicative._Pure[Identity] {
        override def bind[A, B](a: A)(f: A => B): B = f(a)
        override def pure[A](a: A): A = a
      }
      Monad.fromBindPure(x, x)
    }
    def apply[A](a: A): Identity[A] = a
    def run[A](a: Identity[A]): A = a
    def coercion[A, $A0]: Equality[A, Alias.Identity[$A0], Identity[A], Identity[$A0]] = Equality.refl
  }

  val Newtype: Module = Alias

  class RunSyntax[A](val self: Identity[A]) {
    def run: A = Newtype.run(self)
  }

  trait TopLevel {
    val Identity: Newtype.type = Newtype
    type Identity[A] = Newtype.Identity[A]
    val IdentityAlias: Alias.type = Alias
    type IdentityAlias[A] = Alias.Identity[A]
    implicit def toIdentityRunSyntax[A](a: Identity[A]): RunSyntax[A] = new RunSyntax[A](a)
  }

}
~~~

`Newtype` is annotated to only have type `Module`, rather than `Alias.type`, so
`Newtype.Identity[A]` is abstract, and doesn't reduce to `A`.

The following trait is then defined and mixed into the package object:

~~~scala
trait IdentityModule extends IdentityType.TopLevel {
  type Id[A] = IdentityAlias[A]
  implicit val identityMonad: Monad[Identity] = Identity.monad
}
~~~
