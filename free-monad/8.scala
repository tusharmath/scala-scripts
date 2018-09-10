import scala.language.higherKinds
import scala.language.implicitConversions
import scala.util._

object Main extends App {
  trait Monad[F[_]] {
    def flatMap[A, B](fa: F[A])(aFb: A => F[B]): F[B]
    def map[A, B](fa: F[A])(ab: A => B): F[B]
  }

  implicit class ProgramSyntax[F[_]: Monad, A](a: F[A]) {
    private def m = implicitly[Monad[F]]
    def flatMap[B](aFb: A => F[B]): F[B] = m.flatMap(a)(aFb)
    def map[B](ab: A => B): F[B] = m.map(a)(ab)
  }

  class Free[F[_], A] {
    def flatMap[B](aFb: A => Free[F, B]): Free[F, B] = Free.flatMap(this)(aFb)
    def map[B](ab: A => B): Free[F, B] = Free.map(this)(ab)
  }
  object Free {
    def flatMap[F[_], A, B](fa: Free[F, A])(aFb: A => Free[F, B]): Free[F, B] = fa match {
      case FlatMap(dsla, aFx) => Free.flatMap(aFx(dsla))(aFb)
      case Return(a) => aFb(a)
    }
    def map[F[_], A, B](fa: Free[F, A])(ab: A => B): Free[F, B] = fa match {
      case FlatMap(dsla, aFb) => Free.map(aFb(dsla))(ab)
      case Return(a) => Return(ab(a))
    }
  }

  case class FlatMap[F[_], A, B](dsl: F[A], ab: A => Free[F, B]) extends Free[F, B]
  case class Return[F[_], A](a: A) extends Free[F, A]

  implicit def lift[F[_], A](fa: F[A]): Free[F, A] = FlatMap(fa, (a: A) => Return(a))

  trait DSL[A]
  case class Tell(statement: String) extends DSL[Unit]
  case class Ask(question: String) extends DSL[String]

  type ConsoleDSL[A] = Free[DSL, A]

  def app4(): ConsoleDSL[Unit] = for {
    _     <- Tell("Greetings!")
    name  <- Ask("whats your name?")
    -     <- Tell(s"welcome $name!")
  } yield ()

  trait Executor[F[_]] {
    def apply[A](program: F[A]): A
  }

  def interpret[F[_], A, B](f: Free[F, A])(execute: Executor[F]): A = f match {
    case FlatMap(program, afb) => interpret(afb(execute(program)))(execute)
    case Return(a) => a
    case _ => throw new Exception("WTF!")
  }

  interpret(app4())(new Executor[DSL]{
    def apply[A](dsl: DSL[A]): A = dsl match {
      case Tell(statement) => println(statement)
      case Ask(question) => {
        println(question)
        scala.io.StdIn.readLine()
      }
    }
  })
}