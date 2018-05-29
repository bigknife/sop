package bigknife.sop

import java.text.{SimpleDateFormat => SDF}
import java.util.Date

import cats.Id
import cats.data.Kleisli

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import bigknife.sop.implicits._


trait MyTestSuite {
  def now(): String = new SDF("yyyyMMdd-HH:mm:ss,SSS").format(new Date())

  val log: Log[Log.Op]                      = Log[Log.Op]
  val validation: Validation[Validation.Op] = Validation[Validation.Op]

  val logIdInterpreter: NT[Log.Op, Id] = new NT[Log.Op, Id] {
    override def apply[A](fa: Log.Op[A]): Id[A] = fa match {
      case Log.Debug(str) ⇒ println(s"[DEBUG] $str")
    }
  }
  val logIdParInterpreter: NT[Log.Op, Kleisli[Future, Any, ?]] = new NT[Log.Op, Kleisli[Future, Any, ?]] {

    override def apply[A](fa: Log.Op[A]): Kleisli[Future, Any, A] = fa match {
      case Log.Debug(msg) ⇒
        Kleisli { _: Any ⇒
          Future {
            println(s"[DEBUG] [${now()}] $msg")
          }
        }
    }
  }

  val validationSeqInterpreter: NT[Validation.Op, Id] = new NT[Validation.Op, Id] {
    override def apply[A](fa: Validation.Op[A]): Id[A] = fa match {
      case Validation.IsEmail(address) ⇒
        val ret = address.contains("@") && address.contains(".")
        println(s"$address is email? $ret")
        Thread.sleep(1000)
        ret
      case Validation.OverMaxLength(address, maxLength) ⇒
        val ret = address.length <= maxLength
        println(s"$address is over max length($maxLength)? $ret")
        Thread.sleep(1000)
        ret
    }
  }

  val validationParInterpreter: NT[Validation.Op, Kleisli[Future, Any, ?]] =
    new NT[Validation.Op, Kleisli[Future, Any, ?]] {

    override def apply[A](fa: Validation.Op[A]): Kleisli[Future, Any, A] = fa match {
      case Validation.IsEmail(address) ⇒
        Kleisli { x: Any ⇒
          Future {
            val ret = address.contains("@") && address.contains(".")
            println(s"${now()} validating IsEmail($address): $ret")
            Thread.sleep(1000)
            ret
          }
        }
      case Validation.OverMaxLength(address, maxLength) ⇒
        Kleisli { x: Any ⇒
          Future {
            val ret = address.length <= maxLength
            println(s"${now()} $address is over max length($maxLength)? $ret")
            Thread.sleep(1000)
            ret
          }
        }
    }
  }

  def test(): Unit = {
    println("---------------TEST LOG---------------------")
    testLog()
    println("---------------Validation LOG SEQ---------------------")
    testSeqValidation()
    println("---------------Validation LOG PAR---------------------")
    testParValidation()
    println("--------------- Coproduct ----------------------")
    testCoproduct()
  }

  private[this] def testLog(): Unit = {
    import log._
    val program = for {
      _ ← debug("SOP 框架实例")
      _ ← debug("SOP is Sequencial Over Parallel")
      _ ← debug("SOP based on Free and FreeApplicative")
      _ ← debug("SOP 也是套路")
    } yield ()

    program.foldMap(logIdInterpreter)
  }

  private[this] def testSeqValidation(): Unit = {
    import validation._
    import cats.implicits._
    def validationProgram(str: String, maxLength: Int): P[Validation.Op, Boolean] =
      (isEmail(str) |@| overMaxLength(str, maxLength)).map(_ && _)
    val ret: Id[Boolean] = validationProgram("songzenghui@gmail.com", 5).foldMap(validationSeqInterpreter)
    println(s"the final ret is $ret")
    println("--------use impilcit transformation to SOP------------")
    def lift2SopProgram(str: String, maxLength: Int): SP[Validation.Op, Boolean] = {
      for {
        a ← isEmail(str)
        b ← overMaxLength(str, maxLength)
      } yield a && b
    }
    val ret1: Id[Boolean] = lift2SopProgram("songzenghui@gmail.com", 5).foldMap(validationSeqInterpreter)

    println(s"the final ret is $ret1")

  }

  private[this] def testParValidation(): Unit = {
    import cats.implicits._
    import validation._

    import scala.concurrent.ExecutionContext.Implicits.global
    case object Dummy

    def validationProgram(str: String, maxLength: Int): P[Validation.Op, Boolean] =
      (isEmail(str) |@| overMaxLength(str, maxLength)).map(_ && _)
    val retFunc: Kleisli[Future, Any, Boolean] =
      validationProgram("songzenghui@gmail.com", 5).foldMap[Kleisli[Future, Any, ?]](validationParInterpreter)
    val retFuture = retFunc.run(Dummy)
    val ret       = Await.result(retFuture, Duration.Inf)
    println(s"the final ret is $ret")
  }

  private[this] def testCoproduct(): Unit = {
    import cats.data._
    import cats.implicits._
    import cats.syntax._
    type ValidateLogApp[A] = EitherK[Validation.Op, Log.Op, A]
    val coLog        = Log[ValidateLogApp]
    val coValidation = Validation[ValidateLogApp]

    def validationPar(str: String, maxLength: Int): P[ValidateLogApp, Boolean] =
    (coValidation.isEmail(str) |@| coValidation.overMaxLength(str, maxLength)).map[Boolean](_ && _)

    def validate(str: String, maxLength: Int): SP[ValidateLogApp, Boolean] =
      for {
        _ ← coLog.debug(s"validate $str is a email and the max length 1")
        _ ← coLog.debug(s"validate $str is a email and the max length 2")
        _ ← coLog.debug(s"validate $str is a email and the max length 3")
        //b ← validation(str, maxLength)
        b ← validationPar(str, maxLength)
        _ ← coLog.debug(s"validation result is $b")
      } yield true

    val program                                                  = validate("songzenghui@gmail.com", 5)
    implicit val interpreter: NT[ValidateLogApp, Kleisli[Future, Any, ?]] =
      validationParInterpreter or logIdParInterpreter

    val b = program.foldMap(interpreter)
    val ret = Await.result(b.run(1), Duration.Inf)
    println(s"final ret is $ret")
  }

}

object MyTestSuite extends MyTestSuite