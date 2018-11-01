/*
 * https://blog.scalac.io/exploring-tagless-final.html
 * https://www.youtube.com/watch?v=IhVdU4Xiz2U
 */

import scala.language.higherKinds
import scala.concurrent.duration._

object Main extends App {
  trait Monad[F[_]] {
    def map[A, B](a: F[A], ab: A => B): F[B]
    def flatMap[A, B](a: F[A], aFb: A => F[B]): F[B]
  }

  // Needed for "for" operator to work.
  implicit class MonadOps[F[_]: Monad, A](program: F[A]) {
    def F                                = implicitly[Monad[F]]
    def map[B](ab: A => B): F[B]         = F.map(program, ab)
    def flatMap[B](aFb: A => F[B]): F[B] = F.flatMap(program, aFb)
  }

  // Framework code
  case class IO[A](unsafeRun: () => A)

  implicit object IOMonad extends Monad[IO] {
    def map[A, B](a: IO[A], ab: A => B): IO[B] = {
      IO(() => ab(a.unsafeRun()))
    }
    def flatMap[A, B](a: IO[A], aFb: A => IO[B]): IO[B] = {
      aFb(a.unsafeRun())
    }
  }

  // Timing
  trait Time[F[_]] {
    def delay(duration: Duration): F[Unit]
  }

  // Console (DSL for the dev)
  trait Console[F[_]] {
    def ask(question: String): F[String]
    def say(statement: String): F[Unit]
  }

  // Math
  trait Math[F[_]] {
    def random(max: Int): F[Int]
  }

  def program[F[_]: Monad](C: Console[F], M: Math[F], T: Time[F]): F[Unit] =
    for {
      _    <- C.say("Greeting!")
      name <- C.ask("What's your name?")
      -    <- C.say(s"Welcome to the world of functional programming $name")
      num  <- M.random(5)
      _    <- C.say(s"Random number: $num")
    } yield ()

  val timeIO = new Time[IO] {
    def delay(duration: Duration): IO[Unit] = ???
  }

  val consoleIO = new Console[IO] {
    def ask(question: String) =
      IO(() => {
        println(question)
        scala.io.StdIn.readLine()
      })

    def say(statement: String) = IO(() => println(statement))
  }

  val mathIO = new Math[IO] {
    def random(max: Int) = IO(() => scala.util.Random.nextInt(max))
  }

  implicit val ioProgram = new Monad[IO] {
    def map[A, B](a: IO[A], ab: A => B): IO[B]          = a.map(ab)
    def flatMap[A, B](a: IO[A], aFb: A => IO[B]): IO[B] = a.flatMap(aFb)
  }

  program(consoleIO, mathIO, timeIO).unsafeRun()
}
