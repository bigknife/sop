package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

/**
  * @sps implementation
  */
object SPSMacroImpl {
  def withSPSCompanion(t: Defn.Trait): Term.Block = {
    val companionStats = Seq[Stat]() ++ typeOps(t) ++ impl(t) ++ handler(t)
    q"""
    $t
    ${MacroUtils.companion(t.name.value, companionStats)}
    """
  }
  /**
    * generate Op for trait unimplementedMethod
    */
  def typeOps(t: Defn.Trait): Seq[Stat] = {
    val vals = unimplementedVals(t)
    vals.size match {
      case 0 => Seq[Stat]()
      case 1 => vals.map{
        case x@ q"val $_: $m[$mp]" =>
          q"""
          type Op[A] = ${Type.Name(nameOfStat(x) + ".Op")}
          """
      }
      case 2 => (vals(0), vals(1)) match {
        case (x@ q"val $_: $xm[$xmp]", y@ q"val $_: $ym[$ymp]")=>
          Seq(q"""type Op[A] = _root_.cats.data.EitherK[${Type.Name(nameOfStat(x) + ".Op")}, ${Type.Name(nameOfStat(y) + ".Op")},  A]""")
      }
      case x =>
        vals.foldLeft((Seq[Stat](), None: Option[Stat], None:Option[Stat], 0))((acc, a) => a match {
          case x@ q"val $_: $m[$mp]" if acc._2.isEmpty =>
            (acc._1, Some(x), None, 0)
          case x@ q"val $_: $m[$mp]" if acc._3.isEmpty =>
            val stat = q"""
                     type ${Type.Name(s"_Op${acc._4}")}[A] = _root_.cats.data.EitherK[${Type.Name(nameOfStat(acc._2.get) + ".Op")},${Type.Name(m.syntax+".Op")},  A]
        """
            (acc._1 :+ stat, Some(x), Some(stat), acc._4 + 1)

          case x@ q"val $_: $m[$mp]" =>
            //logger.elem(s"${acc._4}, ${vals.size}, ${(acc._4 == vals.size)}")
            val stat = q"""
                    type ${if(acc._4 == vals.size - 2) Type.Name("Op") else Type.Name(s"_Op${acc._4}")}[A] = _root_.cats.data.EitherK[${Type.Name(nameOfStat(a) + ".Op")}, ${Type.Name(s"_Op${acc._4-1}")},  A]
        """
            (acc._1 :+ stat, Some(x), Some(stat), acc._4 + 1)


          case x => logger.elem(s"$x"); acc
        })._1
    }
  }
  def nameOfStat(s: Stat): String = s match {
    case q"val $name: $tp[$_]" => tp.syntax
  }

  def impl(t: Defn.Trait): Seq[Stat] = {
    val implCls = ("class Impl[F[_]](" + implicitParamsSyntax(t) + ") extends "+ t.name.value + "[F] {}").parse[Stat].get match {case x: Defn.Class => x}
    val vals = unimplementedVals(t).map{case x@ q"val $name: $typ[$typp]" => s"${x.syntax} = _${name.syntax}"}.map(_.parse[Stat].get)

    val cls = implCls.copy(templ = implCls.templ.copy(stats = Some(vals)))

    //implicit ctor
    val implicitCtor = s"implicit def impl[F[_]](${implicitParamsSyntax(t)}): ${t.name.value}[${t.tparams(0).name.value}] = new Impl[F]".parse[Stat].get

    val summoner = s"def apply[F[_]](implicit M: ${t.name.value}[F]): ${t.name.value}[F] = M".parse[Stat].get

    Seq(cls, implicitCtor, summoner)
  }

  def handler(t: Defn.Trait): Seq[Stat] = {
    val vals = unimplementedVals(t)
    val handlerImpl: String =
      if (vals.length > 1)
        vals.map {case q"val $name: $typ[$typp]" => s"_${name.syntax}_handler"}.foldLeft(("", ""))((acc, a) => if (acc._2.length == 0) ("", a) else if (acc._1.length == 0) (s"${acc._2} or $a", a) else (s"$a or (${acc._1})", a))._1
      else {
        // 只考虑 ==1或者>1的情况，如果是0，则让scalameta抛错
        vals(0) match {
          case q"val $name: $typ[$typp]" => s"_${name.syntax}_handler"
        }
      }

    Seq(s"implicit def handler[M[_]: _root_.cats.Monad](${implicitHandlerParamsSyntax(t, "M")}): _root_.bigknife.sop.NT[({type T[A] = _root_.bigknife.sop.P[Op, A]})#T, M] = $handlerImpl".parse[Stat].get)
  }

  def implicitParamsSyntax(t: Defn.Trait): String =
    "implicit " + unimplementedVals(t).map {case q"val $name: $typ[$typp]" => s"_${name.syntax}:${typ.syntax}[${typp.syntax}]"}.mkString(",")

  def implicitHandlerParamsSyntax(t: Defn.Trait, m: String): String = {
    val vals = unimplementedVals(t)
    if (vals.isEmpty) ""
    else "implicit " + vals.map {case q"val $name: $typ[$typp]" => s"_${name.syntax}_handler:${typ.syntax}.Handler[$m]"}.mkString(",")
  }



  def unimplementedVals(t: Defn.Trait): Seq[Stat] = {
    //t.templ.stats.get.collect { case x@ q"val $_: $_[$tp]" if tp.syntax == t.tparams(0).name.value => x }
    t.templ.stats.get.foldRight((Seq[Stat](), Map[String, Int]()))((a, acc) => a match {
      case x@ q"val $_: $tt[$tp]" if tp.syntax == t.tparams(0).name.value && !acc._2.contains(tt.syntax) => (x +: acc._1, acc._2.updated(tt.syntax, acc._2.get(tt.syntax).getOrElse(0) + 1))
      case _ => acc
    })._1
  }

}