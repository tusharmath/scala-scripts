import scala.language.higherKinds

object Main extends App {

  // Machinery
  object Machinery {
    trait Functor[F[_]] {
      def map[A, B](fa: F[A])(ab: A => B): F[B]
    }

    trait Apply[F[_]] extends Functor[F] {
      def ap[A, B](fa: F[A])(fab: F[A => B]): F[B]
    }

    trait Applicative[F[_]] extends Apply[F] {
      def point[A](a: A): F[A]
    }

    trait Bind[F[_]] extends Apply[F] {
      def bind[A, B](fa: F[A])(afb: A => F[B]): F[B]
    }

    trait Monad[F[_]] extends Applicative[F] with Bind[F]

    trait FunctionK[F[_], G[_]] {
      def apply[A](fa: F[A]): G[A]
    }

    type ~>[F[_], G[_]] = FunctionK[F, G]

    implicit class MonadOps[F[_]: Monad, A](fa: F[A]) {
      def F                               = implicitly[Monad[F]]
      def flatMap[B](ab: A => F[B]): F[B] = F.bind(fa)(ab)
    }

    implicit class ApplicativeOps[F[_]: Applicative, A](fa: F[A]) {
      def AP                          = implicitly[Applicative[F]]
      def map[B](ab: A => B): F[B]    = AP.map(fa)(ab)
      def ap[B](fab: F[A => B]): F[B] = AP.ap(fa)(fab)
      def &&[B](fb: F[B])             = fb.ap(fa.ap(AP.point((a: A) => (b: B) => (a, b))))
    }
  }

  // FREE
  object Free {
    // import Machinery._

    trait FreeAp[F[_], A]

    object FreeAp {
      def lift[F[_], A](fa: F[A]): FreeAp[F, A]                                    = Lift(fa)
      def pure[F[_], A](a: A): FreeAp[F, A]                                        = Pure(a)
      def ap[F[_], P, A](ffp: FreeAp[F, P], ffpa: FreeAp[F, P => A]): FreeAp[F, A] = Ap(ffp, ffpa)
      def map[F[_], A, B](ffa: FreeAp[F, A], ab: A => B): FreeAp[F, B]             = Map(ffa, ab)
    }

    case class Ap[F[_], P, A](ffp: FreeAp[F, P], ffpa: FreeAp[F, P => A]) extends FreeAp[F, A]
    case class Lift[F[_], A](fa: F[A])                                    extends FreeAp[F, A]
    case class Pure[F[_], A](a: A)                                        extends FreeAp[F, A]
    case class Map[F[_], A, B](ffa: FreeAp[F, A], ab: A => B)             extends FreeAp[F, B]
  }

  import Free._
  import Machinery._

  // Actual Program
  trait DSL[A]
  case class Fetch(url: String) extends DSL[String]

  type FreeDSL[A] = FreeAp[DSL, A]
  implicit object dslApplicative extends Applicative[FreeDSL] {
    def map[A, B](fa: FreeDSL[A])(ab: A => B): FreeDSL[B]          = FreeAp.map(fa, ab)
    def ap[A, B](fa: FreeDSL[A])(fab: FreeDSL[A => B]): FreeDSL[B] = FreeAp.ap(fa, fab)
    def point[A](a: A): FreeDSL[A]                                 = FreeAp.pure(a)
  }

  def fetch(url: String): FreeDSL[String]    = FreeAp.lift(Fetch(url))
  def par[A, B](): FreeDSL[A => B => (A, B)] = FreeAp.pure((a: A) => (b: B) => (a, b))

  def program(): FreeDSL[Unit] = {
    val man    = fetch("man")
    val woman  = fetch("woman")
    val couple = man && woman
    couple.map({ case (a, b) => println(a + b) })
  }

  trait Executor[F[_]] {
    def apply[A](program: F[A]): A
  }

  def interpret[F[_], A](f: FreeAp[F, A])(execute: Executor[F]): A = f match {
    case Ap(ffp, ffpa) => {
      val p  = interpret(ffp)(execute)
      val pa = interpret(ffpa)(execute)
      pa(p)
    }
    case Lift(fa)     => execute(fa)
    case Pure(a)      => a
    case Map(ffa, ab) => ab(interpret(ffa)(execute))
  }

  interpret(program())(new Executor[DSL] {
    def apply[A](dsl: DSL[A]): A = dsl match {
      case Fetch(req) => req + "---RESPONSE"
    }
  })
}
