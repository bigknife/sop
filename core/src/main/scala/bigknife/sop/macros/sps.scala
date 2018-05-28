package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

class sps extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case t: Defn.Trait if MacroUtils.isLegalTrait(t) => SPSMacroImpl.withSPSCompanion(t)
      case _ => abort("@sp must annotate an trait with a type param and without companion object, eg: @sp trait SomeTrait[F[_]]")
    }
  }
}