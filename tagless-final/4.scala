/*
 * https://blog.scalac.io/exploring-tagless-final.html
 */


import scala.language.higherKinds

object Main extends App {
  trait Program[F[_]] {
    def map[A, B](a: F[A], ab: A => B): F[B]
    def flatMap[A, B](a: F[A], aFb: A => F[B]): F[B]
  }

  implicit class ProgramSyntax[F[_], A](program: F[A]) {
    def map[B](ab: A => B)(implicit i: Program[F]): F[B] = i.map(program, ab)
    def flatMap[B](aFb: A => F[B])(implicit i: Program[F]): F[B] = i.flatMap(program, aFb)
  }

  // Framework code
  case class IO[A](unsafeRun: () => A) {
    def map[B](ab: A => B): IO[B] = IO(() => ab(unsafeRun()))
    def flatMap[B](aFb: A => IO[B]): IO[B] = IO(() => aFb(unsafeRun()).unsafeRun())
  }

  // Console (DSL for the dev)
  trait Console[F[_]] {
    def ask(question: String): F[String]
    def say(statement: String): F[Unit]
  }
  trait Math[F[_]] {
    def random(max: Int): F[Int]
  }

  def program[F[_]: Program](C: Console[F], M: Math[F]): F[Unit] = for {
      _    <- C.say("Greeting!")
      name <- C.ask("What's your name?")
      -    <- C.say(s"Welcome to the world of functional programming $name")
      num  <- M.random(5)
      _    <- C.say(s"Random number: $num")
    } yield ()

  val consoleIO = new Console[IO] {
    def ask(question: String) = IO(() => {
      println(question)
      scala.io.StdIn.readLine()
    })

    def say(statement: String) = IO(() => println(statement))
  }

  val mathIO = new Math[IO] {
    def random(max: Int) = IO(() => scala.util.Random.nextInt(max))
  }

  implicit val ioProgram = new Program[IO] {
    def map[A, B](a: IO[A], ab: A => B): IO[B] = a.map(ab)
    def flatMap[A, B](a: IO[A], aFb: A => IO[B]): IO[B] = a.flatMap(aFb)
  }

  program(consoleIO, mathIO).unsafeRun()
}