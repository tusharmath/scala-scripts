/*
 * https://blog.scalac.io/exploring-tagless-final.html
 */


import scala.language.higherKinds

object Main extends App {
  // Framework code
  case class IO[A](unsafeRun: () => A) {
    def map[B](ab: A => B): IO[B] = IO(() => ab(unsafeRun()))
    def flatMap[B](aFb: A => IO[B]): IO[B] = IO(() => aFb(unsafeRun()).unsafeRun())
  }

  // Console (DSL for the dev)
  trait Language

  trait Console extends Language {
    def ask(question: String): IO[String]
    def say(statement: String): IO[Unit]
  }

  trait Math extends Language {
    def random(max: Int): IO[Int]
  }

  def program(C: Console, M: Math): IO[Unit] = for {
      _    <- C.say("Greeting!")
      name <- C.ask("What's your name?")
      -    <- C.say(s"Welcome to the world of functional programming $name")
      num  <- M.random(5)
      _    <- C.say(s"Random number: $num")
    } yield ()

  val console = new Console {
    def ask(question: String) = IO(() => {
      println(question)
      scala.io.StdIn.readLine()
    })

    def say(statement: String) = IO(() => println(statement))
  }

  val math = new Math {
    def random(max: Int) = IO(() => scala.util.Random.nextInt(max))
  }

  program(console, math).unsafeRun()
}