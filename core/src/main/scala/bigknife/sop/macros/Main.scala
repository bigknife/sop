package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

class Main extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"object $name { ..$stats }" =>
        MainMacroImpl.expand(name, stats)
      case _ =>
        abort("@main must annotate an object.")
    }
  }
}