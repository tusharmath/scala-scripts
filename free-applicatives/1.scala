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
      def lift[F[_], A](fa: F[A]): FreeAp[F, A]                                 = Lift(fa)
      def pure[F[_], A](a: A): FreeAp[F, A]                                     = Pure(a)
      def ap[F[_], P, A](fp: FreeAp[F, P], fn: FreeAp[F, P => A]): FreeAp[F, A] = Ap(fp, fn)
      def map[F[_], A, B](fa: FreeAp[F, A], ab: A => B): FreeAp[F, B]           = Map(fa)(ab)
    }

    case class Ap[F[_], P, A](fp: FreeAp[F, P], fn: FreeAp[F, P => A]) extends FreeAp[F, A]
    case class Lift[F[_], A](fa: F[A])                                 extends FreeAp[F, A]
    case class Pure[F[_], A](a: A)                                     extends FreeAp[F, A]
    case class Map[F[_], A, B](fa: FreeAp[F, A])(ab: A => B)           extends FreeAp[F, B]
  }

  import Free._
  import Machinery._

  // Actual Program
  trait DSL[A]
  case class Ask(question: String) extends DSL[String]
  case class Say(message: String)  extends DSL[Unit]
  case class Fetch(url: String)    extends DSL[String]
  case class Print(data: Any)      extends DSL[Unit]
  case class DbRead(key: String)   extends DSL[Int]

  type FreeDSL[A] = FreeAp[DSL, A]

  implicit object dslApplicative extends Applicative[FreeDSL] {
    def map[A, B](fa: FreeDSL[A])(ab: A => B): FreeDSL[B]          = FreeAp.map(fa, ab)
    def ap[A, B](fa: FreeDSL[A])(fab: FreeDSL[A => B]): FreeDSL[B] = FreeAp.ap(fa, fab)
    def point[A](a: A): FreeDSL[A]                                 = FreeAp.pure(a)
  }

  def fetch(url: String): FreeDSL[String]    = FreeAp.lift(Fetch(url))
  def dbRead(key: String): FreeDSL[Int]      = FreeAp.lift(DbRead(key))
  def par[A, B](): FreeDSL[A => B => (A, B)] = FreeAp.pure((a: A) => (b: B) => (a, b))

  def program() = {
    val httpResponse   = fetch("google.com")
    val dbReadResponse = dbRead("user-count")
    val bothOFThem     = httpResponse && dbReadResponse
    bothOFThem.map({ case (a, b) => a + b} )
  }

  println(program())
}
