package bigknife.sop

import cats._
import scala.language.higherKinds
import bigknife.sop.implicits._

trait Log[F[_]] {
  def debug(msg: String): P[F, Unit]
}

object Log {
  sealed trait Op[A]
  final case class Debug(msg: String) extends Op[Unit]

  class To[F[_]](implicit I: InjectK[Op, F]) extends Log[F] {
    def debug(msg: String): P[F, Unit] = Debug(msg).liftP.injectP
  }
  implicit def to[F[_]](implicit I: InjectK[Op, F]): Log[F] = new To[F]
  def apply[F[_]](implicit L: Log[F]): Log[F]              = L
}
