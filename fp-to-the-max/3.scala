import scala.io.StdIn._
import scala.util._

object App3 {
  case class IO[A](unsafeRun: () => A) {
    def map[B](f: A => B): IO[B] = IO(() => f(unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(unsafeRun()).unsafeRun())
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
    def empty: IO[Unit] = IO(() => Unit)
  }
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  def putStrLn(line: String) = IO(() => println(line))
  def getStrLn() = IO(() => readLine())
  def nextInt(upper: Int) = IO(() => scala.util.Random.nextInt(upper))

  // Game Related Logic

  def checkContinue(): IO[Boolean] =
    for {
      input <- getStrLn().map(_.toLowerCase)
      cont  <- input match {
        case "y" => IO.point(true)
        case "n" => IO.point(false)
        case _   => checkContinue()
      }
    } yield cont
  def gameLoop(name: String): IO[Unit] =
    for {
      num   <- nextInt(5).map(_ + 1)
      _     <- putStrLn("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStrLn().map(parseInt)
      _     <- input.fold(putStrLn("You did not enter a number"))(guess =>
                  if (guess == num) putStrLn("You guess right, " + name + "!")
                  else putStrLn("You guessed wrong, " + name + "! The number was: " + num)
              )
      _     <- putStrLn("Do you want to continue, " + name + "?")
      cont  <- checkContinue()
      _     <- if (cont) gameLoop(name) else IO.empty
    } yield ()



  def run(): IO[Unit] = {
    for {
      _     <- putStrLn("Whats your name?")
      name  <- getStrLn
      _     <- putStrLn("Hello, " +  name + ", welcome to the game!")
      _     <- gameLoop(name)
    } yield()
  }

  def main(args: Array[String]): Unit = run().unsafeRun()
}

