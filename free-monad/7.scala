import scala.language.higherKinds
import scala.language.implicitConversions

object Main extends App {
  sealed trait Free[F[_], A] {
    def flatMap[B](ab: A => Free[F, B]): Free[F, B]
    def map[B](ab: A => B): Free[F, B]
  }

  case class FlatMap[F[_], A, B](dsl: F[A], ab: A => Free[F, B]) extends Free[F, B] {
    def flatMap[C](bc: B => Free[F, C]): Free[F, C] = {
      FlatMap(dsl, (a: A) => ab(a).flatMap(bc))
    }
    def map[C](bc: B => C): Free[F, C] = {
      flatMap((b: B) => Return(bc(b)))
    }
  }
  case class Return[F[_], A](a: A) extends Free[F, A] {
    def flatMap[B](ab: A => Free[F, B]): Free[F, B] = {
      ab(a)
    }
    def map[B](ab: A => B): Free[F, B] = {
      Return(ab(a))
    }
  }

  implicit def lift[F[_], A](dsl: F[A]): Free[F, A] = FlatMap(dsl, (a: A) => Return(a))

  trait DSL[A]
  case class Tell(statement: String) extends DSL[Unit]
  case class Ask(question: String) extends DSL[String]


  def app2(): Free[DSL, Unit] = {
    FlatMap(Tell("Greetings!"), (_: Unit) => {
      FlatMap(Ask("whats your name?"), (name: String) => {
        FlatMap(Tell(s"welcome $name!"), (_: Unit) => Return(()))
      })
    })
  }

  def app3(): Free[DSL, Unit] = {
    lift(Tell("Greetings!"))
      .flatMap((_: Unit) => {
        lift(Ask("whats your name?"))
          .flatMap((name: String) => {
            lift(Tell(s"welcome $name!"))
              .flatMap((_: Unit) => {
                Return(())
              })
          })
      })
  }

  def app4(): Free[DSL, Unit] = for {
    _    <- Tell("Greetings!")
    name <- Ask("whats your name?")
    -    <- Tell(s"welcome $name!")
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