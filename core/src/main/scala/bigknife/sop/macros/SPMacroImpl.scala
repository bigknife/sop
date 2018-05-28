package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

object SPMacroImpl {
  /**
    * main method. with an companion addition of a trait.
    */
  def withSPCompanion(t: Defn.Trait): Term.Block = {
    val body = (sealedOp(t) ++ implSummon(t)) :+ handler(t)
    q"""
    $t
    ${MacroUtils.companion(t.name.value, body)}
    """
  }

  /**
    * make a sealed Op and Op’s companion with Case classes transformed
    * from the trait’s abstract method
    */
  def sealedOp(t: Defn.Trait): Seq[Stat] = {
    val opTrait = q"""sealed trait Op[A]"""
    val opCompanion = q"""object ${Term.Name("Op")} {}"""

    val opCaseClass = unimplementedMethod(t).map(caseClass)
    Seq(opTrait, opCompanion.copy(templ = opCompanion.templ.copy(stats = Some(opCaseClass))))
  }

  /**
    * the implementation of the trait
    */
  def impl(t: Defn.Trait): Defn.Class = {
    val implCls = q"class Impl[F[_]](implicit I: _root_.cats.InjectK[Op, F]) extends ${Ctor.Ref.Name(t.name.value)}[F] {}"
    val body = unimplementedMethod(t).map {
      case q"def $name[..$tparams](..$params):P[$tp, $ret]" =>
        q"""def $name[..$tparams](..$params):P[$tp, $ret] = {
            (${Term.Name("Op." + name.value.capitalize)}(..${params.map(x => Term.Name(x.name.value))}) :Op[$ret]).liftP.injectP
        }"""
    }
    implCls.copy(templ = implCls.templ.copy(stats = Some(body)))
  }

  /**
    * a summoner to contruct the trait instance. like SomeTrait[SomeTrait.Op]
    */
  def implSummon(t: Defn.Trait): Seq[Stat] = {
    //implicit def impl
    val implicitsCtor = q"implicit def impl[F[_]](implicit I: _root_.cats.InjectK[Op, F]): ${Type.Name(t.name.value)}[F] = new Impl[F]"
    val applyCtor = q"def apply[F[_]](implicit _XXX: ${Type.Name(t.name.value)}[F]) = _XXX"
    Seq(impl(t), implicitsCtor, applyCtor)
  }

  /**
    * make handlers of the trait, NT
    */
  def handler(t: Defn.Trait): Defn.Trait = {
    // apply
    val unim = unimplementedMethod(t)
    val clsNameSeq = unim.map({case q"def $name[..$tparams](..$_):P[$_, $_]" => "Op." + name.value.capitalize})
    val paramsSeq = unim.map {case q"def $_[..$tparams](..$params):P[$_, $_]" => params.map(x => x.name.value)}
    val imMethodSeq = unim.map({case q"def $name[..$tparams](..$_):P[$_, $_]" => name.value})

    val caseSyntax = (clsNameSeq zip paramsSeq zip imMethodSeq) map {
      case ((clsName, params), imMethod) =>
        ("case " + clsName + "(" + params.mkString(", ") + ") => " + imMethod + "(" + params.mkString(", ") + ")")
    }
    val applyMethod = s"override def apply[A](a: Op[A]): F[A] = a match {\n ${caseSyntax.mkString("\n") } \n}".parse[Stat].get
    val handlerTrait = q"""trait Handler[F[_]] extends _root_.bigknife.sop.NT[Op, F] {}"""

    // vitural handler method
    val virtualHandlerMethod = unim map {
      case q"def $name[..$tparams](..$params):P[$tp, $ret]" =>
        q"def $name[..$tparams](..$params): F[$ret] = ???"
    }

    val stats = applyMethod +: virtualHandlerMethod
    handlerTrait.copy(templ = handlerTrait.templ.copy(stats = Some(stats)))
  }

  /**
    * defs to case classes
    */
  def caseClass(stat: Stat): Stat = stat match {
    case q"def $name[..$tparams](..$params): P[$x, $ret]" =>
      /*
      logger.elem(f"$name")
      logger.elem(f"$params")
      logger.elem(f"$x")
      logger.elem(f"$ret")
       */
      val clsName = Type.Name(name.value.capitalize)
      q"case class $clsName[..$tparams](..$params) extends Op[$ret]"
  }

  /**
    * find the abstract methods of the trait
    */
  def unimplementedMethod(t: Defn.Trait): Seq[Stat] = {
    val F = t.tparams(0).name.value
    t.templ.stats.get.collect {
      case t @ q"def $_[..$_](..$_): P[$tp,$_]" if tp.syntax == F  =>  t
    }
  }

}