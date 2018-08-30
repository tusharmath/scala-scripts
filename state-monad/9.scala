import scala.language.higherKinds

object REPL extends App {
  type State[A, S] = S => (A, S)

  trait Monad[F[_]] {
    def pure[A](a: A): F[A]
    def map[A, B](a: F[A])(ab: A => B): F[B]
    def flatMap[A, B](a: F[A])(aFb: A => F[B]): F[B]
  }

  implicit class ProgramSyntax[F[_], A](program: F[A]) {
    def map[B](ab: A => B)(implicit monad: Monad[F]): F[B] = monad.map(program)(ab)
    def flatMap[B](aFb: A => F[B])(implicit monad: Monad[F]): F[B] = monad.flatMap(program)(aFb)
  }

  type RandomTuple[A] = State[A, Random.Generator]

  object Random {

    case class Generator(seed: Long) {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], Generator(seed2))
      }
    }

    def randomInt: RandomTuple[Int] = {
      generator: Generator => generator.nextInt
    }

    implicit val randomMonad: Monad[RandomTuple] = new Monad[RandomTuple] {
      def pure[A](a: A): RandomTuple[A] = {
        generator: Random.Generator => (a, generator)
      }

      def map[A, B](tup: RandomTuple[A])(ab: A => B): RandomTuple[B] ={
        flatMap(tup)(i => pure(ab(i)))
      }

      def flatMap[A, B](tup: RandomTuple[A])(aFb: A => RandomTuple[B]): RandomTuple[B] = {
        generator: Random.Generator => {
          val (a, gen) = tup(generator)
          aFb(a)(gen)
        }
      }
    }
  }

  def randomPositiveInt: RandomTuple[Int] = for {
    n <- Random.randomInt
  } yield n.abs

  def randomRange(min: Int, max: Int): RandomTuple[Int] = for {
    n <- randomPositiveInt
  } yield min + n % (max - min)

  def randomChar: RandomTuple[Char] = for {
    n <- randomRange(65, 90)
  } yield n.toChar


  def randomList(length: Int): RandomTuple[List[Int]] = {
    val monad = implicitly[Monad[RandomTuple]]
    for {
      a <- Random.randomInt
      list <- if (length == 0) monad.pure(Nil) else randomList(length - 1)
    } yield a :: list
  }



  val basicGenerator = Random.Generator(10101010)
  println(randomPositiveInt(basicGenerator))
  println(randomRange(2, 400)(basicGenerator))
  println(randomChar(basicGenerator))
  println(randomList(5)(basicGenerator))
  println(Random.randomInt(basicGenerator))
}
