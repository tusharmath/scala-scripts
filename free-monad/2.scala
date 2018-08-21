import scala.language.higherKinds

sealed trait IO[T]
object IO {
  val zero = new IO[Unit] {}
}
case class Ask(question: String) extends IO[String]
case class Tell(sentence: String) extends IO[Unit]
case class Chain[A, B](a: IO[A], fab: A => IO[B]) extends IO[B]
case class Map[A, B](a: IO[A], fab: A => B) extends IO[B]

object Main extends App {
  implicit class Enrich[A](io: IO[A]) {
    def flatMap[B](fab: A => IO[B]): IO[B] = Chain(io, fab)
    def map[B](fab: A => B): IO[B] = Map(io, fab)
    def io(): IO[A] = io
  }

  def app2() = {
    for {
      _    <- Tell("Greeting!")
      name <- Ask("What's your name")
      _    <- Tell(s"Welcome to the world of functional programming $name!")
    } yield ()
  }


  def execute[T](io: IO[T]): T = {
    io match {
      case Tell(sentence) => println(sentence)
      case Ask(question) => {
        println(question + ": ")
        scala.io.StdIn.readLine()
      }
      case Chain(a, fab) => execute(fab(execute(a)))
      case Map(a, fab) => fab(execute(a))
      case _ => throw new Exception("WTF!")
    }
  }


  val program = app2().io()
  execute(program)
}