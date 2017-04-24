package scalaFP
package meta

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, compileTimeOnly}

/*
 TODO:
   add annotation for printing out the result of a macro
   caseclassey should allow concrete members:
     default definitions for non-final members should be put into the apply method in the companion
     final members should be kept in the class
   should be possible to specify a name other than "apply" for the constructing method in the companion
   caseclassy should perhaps generate a regular class, rather than a case class
   caseclassy could allow supertypes?
   classyLenses should generate syntax
   newtype macro should handle implicits by putting them in TopLevel, but only for the newtype, not the alias
   when generating a trait/class, macros should add definitions to it if it already exists, rather than replace it
   typeclass macro should generate syntax
   use macros to remove syntax cost
   investigate adding a macro to dealias types https://issues.scala-lang.org/browse/SI-8286
*/

@compileTimeOnly("newtype annotation wasn't removed by macro paradise")
class newtype extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroBundle.newtypeImpl
}

@compileTimeOnly("typeclass annotation wasn't removed by macro paradise")
class typeclass extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroBundle.typeclassImpl
}

@compileTimeOnly("classyLenses annotation wasn't removed by macro paradise")
class classyLenses extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroBundle.classyLensesImpl
}

@compileTimeOnly("lenses annotation wasn't removed by macro paradise")
class lenses extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroBundle.lensesImpl
}

@compileTimeOnly("caseclassy annotation wasn't removed by macro paradise")
class caseclassy extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro MacroBundle.caseclassyImpl
}

//TODO use of collect may be wrong
class MacroBundle(val c: Context) {
  import c.universe._

  def fail(s: String): Nothing = c.abort(c.enclosingPosition, s)

  def output(outputs: List[c.Tree]): c.Expr[Any] = c.Expr[Any](Block(outputs, Literal(Constant(()))))

  def modifyModuleBody(f: List[Tree] => List[Tree], obj: ModuleDef): ModuleDef = {
    val q"$mods object $name extends ..$bases { $self => ..$body }" = obj
    q" $mods object $name extends ..$bases { $self =>..${f(body)} } "
  }

  def newtypeImpl(annottees: c.Tree*): c.Expr[Any] = {
    annottees.toList match {
      case List(obj: ModuleDef) =>
        val newObj = modifyModuleBody(newtypeDefs, obj)
        // println(newObj)
        output(List(newObj))
      case _ => fail("newtype annottee must be an object")
    }
  }

  def classCompanion(annottees: Seq[c.Expr[Any]]): ClassCompanion = {
    val (x, y) = annottees.map(_.tree).toList match {
      case List(clss: ClassDef, comp: ModuleDef) => (clss, comp)
      case List(clss: ClassDef) => (clss, q"object ${TermName(clss.name.toString)}")
      case _ => fail("not a class")
    }
    ClassCompanion(x, y, Config())
  }

  def classyLensesImpl(annottees: c.Expr[Any]*): c.Expr[Any] =
    classCompanion(annottees).genClassyLenses.block

  def lensesImpl(annottees: c.Expr[Any]*): c.Expr[Any] =
    classCompanion(annottees).modConfig(_.copy(lensName = identity)).genLenses.block

  def typeclassImpl(annottees: c.Expr[Any]*): c.Expr[Any] = classCompanion(annottees).typeclass.block

  def caseclassyImpl(annottees: c.Expr[Any]*): c.Expr[Any] = classCompanion(annottees).caseclassy.block

  case class Config(
    lensName: String => String = "_" + _,
    selfLensName: String = "lens",
    classyLensName: String => String = identity,
    defTypeName: String => String = "_" + _.capitalize,
    defFieldName: String => String = identity
  )

  case class ClassCompanion(clss: ClassDef, comp: ModuleDef, config: Config) {
    val cname = clss.name
    val ctparams = clss.tparams
    val ctparamNames = clss.tparams.map(_.name)
    val ctype = tq"$cname[..$ctparamNames]"
    lazy val caseAccessors: List[ValDef] = clss.collect {
      case (m: ValDef) if m.mods.hasFlag(Flag.CASEACCESSOR) => m
    }

    val oname = comp.name

    def addDefs(defs: List[DefTree]): ClassCompanion = copy(comp = modifyModuleBody(_ ::: defs, comp))

    def modConfig(f: Config => Config) = copy(config = f(config))

    def block: c.Expr[Any] = {
      // println(clss)
      // println(comp)
      output(List(clss, comp))
    }

    def genTypeclassApply: ClassCompanion = addDefs(List(q"def apply[..$ctparams](implicit e: $ctype): $ctype = e"))

    def lensName(nm: TermName): TermName = TermName(config.lensName(nm.toString))

    def genLenses: ClassCompanion = {
      val lenses = caseAccessors.map { m =>
        val nm = m.name
        q"def ${lensName(nm)}[..$ctparams]: Lens_[$ctype, ${m.tpt}] = Lens(_.$nm, x => _.copy($nm = x))"
      }
      addDefs(lenses)
    }

    def selfLensName = TermName(config.selfLensName)
    def classyLensName(nm: TermName) = TermName(config.classyLensName(nm.toString))

    def genClassyLenses: ClassCompanion = {
      val selfLens = q"def $selfLensName[..$ctparams, T](implicit e: Has[$ctype, T]): Lens_[T, $ctype] = Has.lens"
      val lenses = caseAccessors.map { m =>
        val nm = m.name
        q"""
        def ${classyLensName(nm)}[..$ctparams, T](implicit e: Has[$ctype, T]): Lens_[T, ${m.tpt}] =
          $selfLensName.composeL(${lensName(nm)})
      """
      }
      genLenses.addDefs(selfLens :: lenses)
    }

    def defTypeName(dd: DefDef) = TypeName(config.defTypeName(dd.name.toString))
    def defFieldName(dd: DefDef) = TermName(config.defFieldName(dd.name.toString))

    def caseclassy: ClassCompanion = {
      val members = clss match {
        case q"$_ trait $name[$_] {..$stats}" =>
          stats.collect {
            //TODO is this exhaustive?
            case (m: MemberDef) =>
              if(!m.mods.hasFlag(Flag.ABSTRACT | Flag.DEFERRED))
                fail("typeclass can only contain abstract members")
              else m
          }
        case _ => fail("typeclass must be a trait with no superclasses")
      }

      val defs = members.collect { case (dd: DefDef) => dd }
      val vals = members.collect { case (vd: ValDef) => vd }

      val mtraits = defs.flatMap { dd =>
        val mtname = defTypeName(dd)
        val mtrait = q"""
          trait $mtname[..$ctparams] {
            $dd
          }
        """
        val q"$mods def $dname[..$dtparams](...$dparamss): $tpt" = dd
        //TODO keep modifiers, except deferred
        val mcomp = q"""
          object ${TermName(mtname.toString)} {
            implicit class Syntax[..$ctparams](val self: $mtname[..$ctparamNames]) extends AnyVal {
              def apply[..$dtparams](...$dparamss): $tpt = self.$dname[..${dtparams.map(_.name)}](...${dparamss.map(_.map(_.name))})
            }
          }
        """
        List(mtrait, mcomp)
      }
      val mparams = defs.map { dd =>
        q"""val ${defFieldName(dd)}: $oname.${defTypeName(dd)}[..$ctparamNames]"""
      }
      val newClss = q"""case class $cname[..$ctparams](..${mparams ::: vals})"""
      copy(clss = newClss).addDefs(mtraits)
    }

    def typeclass: ClassCompanion = genTypeclassApply.caseclassy.genClassyLenses
  }

  //// Newtype Macro /////////////////////////////////////////////////////////////////////////

  def addFlags(m: Modifiers, s: FlagSet): Modifiers = {
    val Modifiers(flags, privateWithin, annotations) = m
    Modifiers(flags | s, privateWithin, annotations)
  }

  def wDeferred(m: Modifiers): Modifiers = addFlags(m, Flag.DEFERRED)

  def defer(x: c.Tree): c.Tree = {
    x match {
      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        DefDef(wDeferred(mods), name, tparams, vparamss, tpt, EmptyTree)
      case ValDef(mods, name, tpt, rhs) =>
        ValDef(wDeferred(mods), name, tpt, EmptyTree)
      case TypeDef(mods, name, tparams, rhs) =>
        q"${wDeferred(mods)} type $name[..$tparams]"
      case other => other
    }
  }

  //TODO avoid name capture
  def cloneTParams(xs: List[TypeDef]): List[TypeDef] = {
    def rename(x: TypeName): TypeName = TypeName("$" + x.toString + "0")
    xs.map {
      case TypeDef(mods, name, tparams, rhs) =>
        TypeDef(mods, rename(name), tparams, rhs)
    }
  }

  def newtypeDefs(stats: List[Tree]): List[DefTree] = {
    stats match {
      case (td@TypeDef(mods, name, tparams, rhs)) :: _ =>
        val tparamNames = tparams.map(_.name)
        val typ = tq"$name[..$tparamNames]"
        val tparams1 = cloneTParams(tparams)
        val tparamNames1 = tparams1.map(_.name)
        //TODO find a way to do this without using the alias
        val rhs1 = tq"Alias.$name[..$tparamNames1]"
        //TODO add mods to the generated defs
        val newStats = stats ::: List(
          q"def apply[..$tparams](a: $rhs): $typ = a",
          q"def run[..$tparams](a: $typ): $rhs = a",
          q"""def coercion[..${tparams ::: tparams1}]
                 : Equality[$rhs, $rhs1, $typ, $name[..$tparamNames1]] = Equality.refl"""

        )
        val module = q"""
          trait Module {
            ..${newStats.map(defer)}
          }
        """
        val impl = q"""
          object Alias extends Module {
            ..$newStats
          }
        """
        val newtype = q"val Newtype: Module = Alias"
        val syntax = q"""
          class RunSyntax[..$tparams](val self: $rhs) extends AnyVal {
            def run: $rhs = self
          }
        """
        val termName = TermName(name.toString)
        val aliasName = name.toString + "Alias"
        val topLevel = q"""
          trait TopLevel {
            val $termName: Newtype.type = Newtype
            $mods type $name[..$tparams] = Newtype.$name[..$tparamNames]
            val ${TermName(aliasName)}: Alias.type = Alias
            $mods type ${TypeName(aliasName)}[..$tparams] = Alias.$name[..$tparamNames]
            implicit def ${TermName("to" + name.toString + "RunSyntax")}[..$tparams](a: $name[..$tparamNames])
              : RunSyntax[..$tparamNames] = new RunSyntax[..$tparamNames](Newtype.run(a))
          }
        """
        List(module, impl, newtype, syntax, topLevel)
      case _ => fail("first statement of newtype annottee must be a type alias")
    }
  }
}
