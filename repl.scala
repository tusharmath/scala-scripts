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
    def flatMap[B](fab: A => IO[B]) = Enrich(Chain(io, fab))
    def map[B](fab: A => B) = Enrich(Map(io, fab))
    def point = io
  }

  def app2 () = {
    Tell("Greetings!\n\n")
      .flatMap(_ => Tell("Welcome to the world of functional programming!"))
      .flatMap(_ => Ask("Whats your name?"))
      .flatMap(name => Tell(s"Hello $name!"))
  }


  def execute[T](io: IO[T]): T = {
    io match {
      case Tell(sentence) => println(sentence)
      case Ask(question) => {
        println(question + ": ")
        scala.io.StdIn.readLine()
      }
      case Chain(a, fab) => {
        execute(fab(execute(a)))
      }
      case _ => throw new Exception("WTF!")
    }
  }


  val program = app2().point
  execute(program)
}