package bigknife.sop.effect
import bigknife.sop._
import bigknife.sop.implicits._
import bigknife.sop.macros._

/**
  * error SOP from frees.io
  * @see https://insight.io/github.com/frees-io/freestyle/blob/master/modules/effects/shared/src/main/scala/effects/error.scala?source=0&line=33
  */
object error {
  trait Catchable[F[_]] {
    def handle[A](t: Throwable): F[A]
  }

  object Catchable {
    def apply[F[_]](implicit C: Catchable[F]): Catchable[F] = C
  }
  @sp
  trait ErrorM[F[_]] {
    def either[A](fa: _root_.scala.Either[Throwable, A]): P[F, A]
    def throws[A](t: Throwable): P[F, A]
    def catchNonFatal[A](a: _root_.cats.Eval[A]): P[F, A]
  }
  trait ErrorMInstance {
    import cats.MonadError
    import cats.data.Kleisli

    implicit val optionCatchable = new Catchable[Option] {
      def handle[A](t: Throwable): Option[A] = None
    }
    implicit val eitherCatchable = new Catchable[({type λ[T] = Either[Throwable, T]})#λ] {
      def handle[A](t: Throwable): Either[Throwable, A] = Left(t)
    }

    implicit def errorMHandler[M[_]](implicit M: MonadError[M, Throwable]): ErrorM.Handler[M] =
      new ErrorM.Handler[M] {
        override def either[A](fa: _root_.scala.Either[Throwable, A]): M[A] = fa match {
          case Left(t)  => M.raiseError(t)
          case Right(a) => M.pure(a)
        }
        override def throws[A](t: Throwable): M[A]                  = M.raiseError(t)
        override def catchNonFatal[A](a: _root_.cats.Eval[A]): M[A] = M.catchNonFatal[A](a.value)
      }
    implicit val idME = new MonadError[cats.Id, Throwable] {
      def pure[A](x: A): cats.Id[A]                                                  = x
      def handleErrorWith[A](fa: cats.Id[A])(f: Throwable => cats.Id[A]): cats.Id[A] = fa
      def raiseError[A](e: Throwable): cats.Id[A]                                    = throw e
      def flatMap[A, B](fa: cats.Id[A])(f: A => cats.Id[B]): cats.Id[B]              = f(fa)
      def tailRecM[A, B](a: A)(f: A => cats.Id[Either[A, B]]): cats.Id[B] = f(a) match {
        case Left(a)  => tailRecM(a)(f)
        case Right(b) => b
      }
    }
    implicit val eitherME =
      new MonadError[({ type λ[A] = Either[Throwable, A] })#λ /*Either[Throwable, ?]*/, Throwable] {
        def pure[A](x: A): Either[Throwable, A] = Right(x)
        def handleErrorWith[A](fa: Either[Throwable, A])(
          f: Throwable => Either[Throwable, A]): Either[Throwable, A] = fa match {
          case Left(t)  => f(t)
          case Right(a) => Right(a)
        }
        def raiseError[A](e: Throwable): Either[Throwable, A] = Left(e)
        def flatMap[A, B](fa: Either[Throwable, A])(
          f: A => Either[Throwable, B]): Either[Throwable, B] = fa match {
          case Left(t)  => Left(t)
          case Right(a) => f(a)
        }
        def tailRecM[A, B](a: A)(f: A => Either[Throwable, Either[A, B]]): Either[Throwable, B] =
          f(a) match {
            case Left(t)         => Left(t)
            case Right(Left(a))  => tailRecM(a)(f)
            case Right(Right(b)) => Right(b)
          }
      }
    import cats.Monad
    implicit def kleislyME[M[_], R](implicit M: Monad[M], C: Catchable[M]) =
      new MonadError[({ type λ[A] = Kleisli[M, R, A] })#λ, Throwable] {
        def pure[A](x: A): Kleisli[M, R, A] = Kleisli { (r: R) =>
          M.pure(x)
        }
        def handleErrorWith[A](a: Kleisli[M, R, A])(
          f: Throwable => Kleisli[M, R, A]): Kleisli[M, R, A] = Kleisli { (r: R) =>
          try {
            a(r)
          } catch {
            case t: Throwable => f(t)(r)
          }
        }
        def raiseError[A](e: Throwable): Kleisli[M, R, A] = Kleisli { (r: R) =>
          C.handle[A](e)
        }
        def flatMap[A, B](fa: Kleisli[M, R, A])(f: A => Kleisli[M, R, B]): Kleisli[M, R, B] =
          fa.flatMap(f)

        def tailRecM[A, B](a: A)(f1: A => Kleisli[M, R, Either[A, B]]): Kleisli[M, R, B] = {
          def go(r: R, f: Kleisli[M, R, Either[A, B]]): M[B] =
          /* M[Either[A, B]] */
            M.flatMap(f(r)) {
              case Left(a1)  => go(r, f1(a1))
              case Right(b1) => M.pure(b1)
            }
          Kleisli { (r: R) =>
            go(r, f1(a))
          }
        }

      }

  }
  object implicits extends ErrorMInstance
}