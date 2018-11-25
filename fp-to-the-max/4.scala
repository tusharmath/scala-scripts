// https://gist.github.com/jdegoes/1b43f43e2d1e845201de853815ab3cb9
import scala.io.StdIn._
import scala.util._
import scala.language.higherKinds

object App4 {
  // Framework

  // Program
  trait Program[F[_]] {
    def finish[A](a: => A): F[A]
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]
    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  // Console
  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn(): F[String]
  }

  // Random
  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }

  implicit class ProgramSyntax[F[_]: Program, A](fa: F[A]) {
    def map[B](f: A => B): F[A] = fa.map(f)
    def flatMap[B](f: A => F[B]): F[B] = fa.flatMap(f)
  }


  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  }


  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
    def empty: IO[Unit] = IO(() => ())
    implicit val programIO = new Program[IO] {
      def finish[A](a: => A): IO[A] = IO.point(a)
      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)
      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }
    implicit val consoleIO = new Console[IO] {
      def putStrLn(line: String) = IO(() => println(line))
      def getStrLn() = IO(() => readLine())
    }
    implicit val randomIO = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))
    }
  }

  case class TestData(input: List[String], output: List[String], nums: List[Int])


  // NEW
  def putStrLn[F[_]: Console](line: String) = implicitly[Console[F]].putStrLn(line)
  def getStrLn[F[_]: Console]() = implicitly[Console[F]].getStrLn()
  def nextInt[F[_]: Random](upper: Int) = implicitly[Random[F]].nextInt(upper)
  def finish[F[_], A](a: => A)(implicit f: Program[F]): F[A] = f.finish(a)


  // OLD
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  // def putStrLn(line: String): IO[Unit] = IO(() => println(line))
  // def getStrLn(): IO[String] = IO(() => readLine())
  // def nextInt(upper: Int) = IO(() => scala.util.Random.nextInt(upper))

  // Game
  def checkContinue[F[_]]()(implicit a: Console[F], b: Program[F]): F[Boolean] =
    for {
      input <- getStrLn().map(_.toLowerCase)
      cont  <- input match {
        case "y" => finish(true)
        case "n" => finish(false)
        case _   => checkContinue()
      }
    } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn()
      _     <- parseInt(input).fold(putStrLn("You did not enter a number"))(guess =>
                  if (guess == num) putStrLn("You guess right, " + name + "!")
                  else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
              )
      _     <- putStrLn("Do you want to continue, " + name + "?")
      cont  <- checkContinue()
      _     <- if (cont) gameLoop(name) else implicitly[Program[F]].finish(())
    } yield ()

  def main[F[_]: Program: Console: Random](): F[Unit] = {
    for {
      _     <- putStrLn("Whats your name?")
      name  <- getStrLn
      _     <- putStrLn("Hello, " +  name + ", welcome to the game!")
      _     <- gameLoop(name)
    } yield()
  }

  def main(args: Array[String]): Unit = {
    main[IO].unsafeRun()
  }
}

