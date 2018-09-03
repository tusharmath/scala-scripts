/*
 * https://blog.scalac.io/exploring-tagless-final.html
 * https://www.youtube.com/watch?v=IhVdU4Xiz2U
 */


import scala.language.higherKinds

object Main extends App {
  // Framework code
  case class IO[A](unsafeRun: () => A) {
    def map[B](ab: A => B): IO[B] = IO(() => ab(unsafeRun()))
    def flatMap[B](aFb: A => IO[B]): IO[B] = IO(() => aFb(unsafeRun()).unsafeRun())
  }

  // Language (DSL for the dev)
  trait Language {
    def ask(question: String): IO[String]
    def say(statement: String): IO[Unit]
  }

  def program(L: Language): IO[Unit] = for {
      _    <- L.say("Greeting!")
      name <- L.ask("What's your name?")
      -    <- L.say(s"Welcome to the world of functional programming $name")
    } yield ()

  val interpreter = new Language {
    def ask(question: String) = IO(() => {
      println(question)
      scala.io.StdIn.readLine()
    })

    def say(statement: String) = IO(() => println(statement))
  }

  program(interpreter).unsafeRun()
}