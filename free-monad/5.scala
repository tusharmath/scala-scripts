import scala.language.higherKinds
import scala.language.implicitConversions

object Main extends App {
  trait Free[F[_], A]
  case class FlatMap[F[_], A, B](program: F[A], afb: A => Free[F, B]) extends Free[F, B]
  case class Mapped[F[_], A, B](program: F[A], ab: A => B) extends Free[F, B]

  class DSL[A] {
    def flatMap[B](fa: A => Free[DSL, B]) = FlatMap(this, fa)
    def map[B](ab: A => B) = Mapped(this, ab)
  }
  case class Tell(statement: String) extends DSL[Unit]
  case class Ask(question: String) extends DSL[String]

  trait Executor[F[_]] {
    def apply[A](program: F[A]): A
  }

  def interpret[F[_], A, B](f: Free[F, A])(execute: Executor[F]): A = f match {
    case FlatMap(program, afb) => interpret(afb(execute(program)))(execute)
    case Mapped(program, ab) => ab(execute(program))
    case _ => throw new Exception("WTF!")
  }

  def app2() = for {
    _     <- Tell("Greetings!")
    name  <- Ask("Whats your name?")
    _     <- Tell(s"Welcome to the world of functional programming $name")
  } yield ()

  interpret(app2())(new Executor[DSL]{
    def apply[A](program: DSL[A]): A = program match {
      case Tell(statement) => println(statement)
      case Ask(question) => {
        println(question)
        scala.io.StdIn.readLine()
      }
    }
  })
}