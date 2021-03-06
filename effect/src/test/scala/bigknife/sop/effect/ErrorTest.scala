package bigknife.sop.effect

import bigknife._
import sop._
import sop.implicits._
import sop.macros._
object ErrorTest extends App {
  import error.ErrorM

  @sp
  trait Log[F[_]] {
    def log(a: String): P[F, Unit]
  }

  @sps
  trait Stack[F[_]] {
    val log: Log[F]
    val error: ErrorM[F]
  }

  val stack = Stack[Stack.Op]
  val p: SP[Stack.Op, Unit] = for {
    _ <- stack.log.log("错误测试")
    x <- stack.error.throws[Unit](new Exception("这会导致短路"))
    _ <- stack.log.log("这不会出现")
  } yield x

  val p2: SP[Stack.Op, Unit] = for {
    _ <- stack.log.log("错误测试")
    x <- stack.error.catchNonFatal[Unit](cats.Eval.later { throw new Exception("这会抓住") })
    _ <- stack.log.log("也不会出现")
  } yield x

  object logHandler {
    implicit val logHandler = new Log.Handler[cats.Id] {
      override def log(a: String): cats.Id[Unit] = println(s"LOG: $a")
    }

    implicit val logEitherHandler = new Log.Handler[Either[Throwable, ?]] {
      override def log(a: String): Either[Throwable, Unit] = Right(println(s"LOG: $a"))
    }
  }

  import error.implicits._
  import logHandler._
  import Stack._
  val s = p.interpret[Either[Throwable, ?]]
  println(s)

  val s1 = p2.interpret[Either[Throwable, ?]]
  println(s1)

}

object ErrorTest1 extends App {
  import error._

  case class User(name: String)

  @sp trait Logger[F[_]] {
    def debug(msg: String): P[F, Unit]
  }

  @sp trait UserService[F[_]] {
    def newUser(name: String): P[F, User]
  }

  @sp trait UserRepo[F[_]] {
    def saveUser(user: User): P[F, Either[Throwable, User]]
  }

  @sps trait ErrorTestApp[F[_]] {
    val error: ErrorM[F]
    val logger: Logger[F]
    val userService: UserService[F]
    val userRepo: UserRepo[F]
  }

  import ErrorTestApp._

  val app = ErrorTestApp[ErrorTestApp.Op]

  def registerApp(name: String): SP[ErrorTestApp.Op, User] =
    for {
      _  <- app.logger.debug("start to register user")
      u  <- app.userService.newUser(name)
      _  <- app.logger.debug(s"user created $u")
      eu <- app.userRepo.saveUser(u)
      u1 <- app.error.either[User](eu)
      _  <- app.logger.debug(s"registered: $u1")
    } yield u1

  //println(registerApp)

  case class Setting(debug: Boolean)

  type Stack[A] = cats.data.Kleisli[Option, Setting, A]
  object Stack {
    def apply[A](f: Setting => Option[A]): Stack[A] = cats.data.Kleisli { (s: Setting) =>
      f(s)
    }
  }
  object handlers {
    implicit val loggerHandler = new Logger.Handler[Stack] {
      override def debug(msg: String): Stack[Unit] = Stack { setting =>
        if (setting.debug) Some(println(s"DEBUG: $msg"))
        else Some(())
      }
    }
    implicit val userServiceHandler = new UserService.Handler[Stack] {
      override def newUser(name: String): Stack[User] = Stack { setting =>
        Some(User(name))
      }
    }
    implicit val userRepohandler = new UserRepo.Handler[Stack] {
      override def saveUser(user: User): Stack[Either[Throwable, User]] = Stack { setting =>
        if (user.name == "fail") Some(Left(new Exception("user's name should not be 'fail'")))
        else Some(Right(user.copy(name = user.name + ":ACCEPTED")))
      }
    }
  }

  import handlers._
  import error.implicits._
  import cats.implicits._

  val k = registerApp("zhangsan").interpret[Stack]
  val r = k(Setting(debug = false))
  println(r)

  val s2 =  for {
    _ <- registerApp("fail")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")
    _ <- registerApp("zhangsan")

  } yield ()

  val k2 = s2.interpret[Stack]
  println(k2(Setting(debug=true)))

}