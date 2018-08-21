import scala.language.higherKinds


object Main extends App {
  class DSL[A] {
    def flatMap[B](fab: A => DSL[B]): DSL[B] = Chain(this, fab)
    def map[B](fab: A => B): DSL[B] = Map(this, fab)
  }
  case class Ask(question: String) extends DSL[String]
  case class Tell(sentence: String) extends DSL[Unit]
  case class Chain[A, B](a: DSL[A], fab: A => DSL[B]) extends DSL[B]
  case class Map[A, B](a: DSL[A], fab: A => B) extends DSL[B]

  def app2() = {
    for {
      _    <- Tell("Greeting!")
      name <- Ask("What's your name")
      _    <- Tell(s"Welcome to the world of functional programming $name!")
    } yield ()
  }


  def execute[T](io: DSL[T]): T = {
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


  val program = app2()
  execute(program)
}