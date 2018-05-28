package bigknife.sop.macros

import scala.meta._
import org.scalameta.logger
import scala.collection.immutable.Seq

object MainMacroImpl {
  def expand(name: Term.Name, stats: Seq[Stat]): Defn.Object = {
    val main = q"def main(args: Array[String]): Unit = { ..$stats }"
    q"object $name { $main }"
  }
}