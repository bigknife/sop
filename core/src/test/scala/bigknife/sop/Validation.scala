package bigknife.sop

import cats._

import scala.language.higherKinds
import bigknife.sop.implicits._

trait Validation[F[_]] {
  def isEmail(address: String): P[F, Boolean]
  def overMaxLength(str: String, maxLength: Int): P[F, Boolean]
}

object Validation {
  sealed trait Op[A]
  final case class IsEmail(address: String)                   extends Op[Boolean]
  final case class OverMaxLength(str: String, maxLength: Int) extends Op[Boolean]

  class To[F[_]](implicit I: InjectK[Op, F]) extends Validation[F] {
    override def isEmail(address: String): P[F, Boolean] =
      IsEmail(address).liftP.injectP

    override def overMaxLength(str: String, maxLength: Int): P[F, Boolean] =
      OverMaxLength(str, maxLength).liftP.injectP
  }
  implicit def to[F[_]](implicit I: InjectK[Op, F]): Validation[F] = new To[F]
  def apply[F[_]](implicit V: Validation[F]): Validation[F]       = V
}
