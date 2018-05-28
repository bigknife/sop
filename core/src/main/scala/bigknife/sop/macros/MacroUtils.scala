package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

object MacroUtils {
  /**
    * a trait with a  type param of `* -> *`
    */
  def isLegalTrait(t: Defn.Trait): Boolean = {
    // with tparams F[_]
    t.tparams.size == 1 && t.tparams(0).tparams.size == 1 && t.templ.stats.isDefined
  }

  /**
    * make a companion object with name and stats
    */
  def companion(name: String, stats: Seq[Stat]): Defn.Object = {
    val o = q"""object ${Term.Name(name)} {}"""
    o.copy(templ = o.templ.copy(stats = Some(stats)))
  }
}