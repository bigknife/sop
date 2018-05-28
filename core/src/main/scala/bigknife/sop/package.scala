package bigknife

/**
  * sop.scala
  * @author songzenghui@gmail.com
  */
import cats.{~>, InjectK, Applicative, Monad}
import cats.free.{Free, FreeApplicative}

package object sop {
  type S[F[_], A]     = Free[F, A]
  type P[F[_], A]     = FreeApplicative[F, A]
  type SP[F[_], A]    = S[({type T[B] = P[F, B]})#T, A] //S[P[F, ?], A]
  type NT[F[_], G[_]] = F ~> G
}

package sop {
  private[sop] object S {
    def pure[F[_], A](a: A): S[F, A] = Free.pure[F, A](a)

    def lift[F[_], A](fa: F[A]): S[F, A] = Free.liftF(fa)
  }

  private[sop] object P {
    def pure[F[_], A](a: A): P[F, A] = FreeApplicative.pure(a)

    def lift[F[_], A](fa: F[A]): P[F, A] = FreeApplicative.lift(fa)
    def inject[F[_], G[_], A](a: P[F, A])(implicit I: InjectK[F, G]): P[G, A] =
      a.compile(new NT[F, G] {
        override def apply[B](b: F[B]): G[B] = I.inj(b)
      })
  }

  private[sop] object SP {
    def pure[F[_], A](a: A): SP[F, A] = S.pure[({type λ[B] = P[F, B]})#λ, A](a)

    def lift[F[_], A](fa: F[A]): SP[F, A] = S.lift(P.lift(fa))
    def inject[F[_], G[_], A](a: SP[F, A])(implicit I: InjectK[F, G]): SP[G, A] =
      a.compile(new NT[/*P[F, ?]*/({type T[B] = P[F, B]})#T, /*P[G, ?]*/({type U[B] = P[G, B]})#U] {
        override def apply[B](b: P[F, B]): P[G, B] =
          b.compile(new NT[F, G] {
            override def apply[C](c: F[C]): G[C] = I.inj(c)
          })
      })
  }

  private[sop] trait LiftSyntax[F[_], A] {
    val fa: F[A]
    def liftS: S[F, A]   = S.lift(fa)
    def liftP: P[F, A]   = P.lift(fa)
    def liftSP: SP[F, A] = SP.lift(fa)
  }

  private[sop] trait PInjectSyntax[F[_], A] {
    val pa: P[F, A]
    def injectP[G[_]](implicit I: InjectK[F, G]): P[G, A] =
      P.inject(pa)
  }

  private[sop] trait SPInterpreterSyntax[F[_], A] {
    val spa: SP[F, A]
    def interpret[M[_]: Monad](implicit nt: NT[/*P[F, ?]*/({type T[B] = P[F, B]})#T, M]): M[A] = spa.foldMap(nt)
  }

  private[sop] trait PInterpreterSyntax[F[_], A] {
    val pa: P[F, A]
    def interpret[M[_]: Monad](implicit nt: NT[F, M]): M[A] = pa.foldMap(nt)
  }

  private[sop] trait syntax {
    implicit def liftSyntax[F[_], A](a: F[A])       = new LiftSyntax[F, A]    { val fa = a }
    implicit def pInjectSyntax[F[_], A](a: P[F, A]) = new PInjectSyntax[F, A] { val pa = a }
    implicit def spInterpreterSyntax[F[_], A](a: SP[F, A]) = new SPInterpreterSyntax[F, A] {val spa = a}
    implicit def pInterpreterSyntax[F[_], A](a: P[F, A]) =new PInterpreterSyntax[F, A] {val pa = a}

    implicit class PureSyntax[A](a: A) {
      def pureP[F[_]]: P[F, A] = P.pure[F, A](a)
      def pureS[F[_]]: S[F, A] = S.pure[F, A](a)
      def pureSP[F[_]]: SP[F, A] = SP.pure[F, A](a)

    }
  }
  //object syntax extends syntax
  private[sop] trait trans {
    implicit def p2sp[F[_], A](a: P[F, A]): SP[F, A] = S.lift(a)
    implicit def liftNT[F[_], G[_]: Applicative](nt: NT[F, G]): NT[/*P[F, ?]*/({type T[A] = P[F, A]})#T, G] =
      new NT[({type T[A] = P[F, A]})#T/*P[F, ?]*/, G] {
        override def apply[A](a: P[F, A]): G[A] = a.foldMap(nt)
      }
  }
  //object trans extends trans

  object implicits extends syntax with trans
  object syntax extends syntax
  object trans extends trans
}