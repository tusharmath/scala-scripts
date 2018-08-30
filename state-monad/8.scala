import scala.language.higherKinds

object REPL extends App {
  implicit class RandomSyntax[A](random: Random.RandomTuple[A]) {
    def map[B](ab: A => B): Random.RandomTuple[B] = Random.map(random)(ab)
    def flatMap[B](ab: A => Random.RandomTuple[B]): Random.RandomTuple[B] = Random.flatMap(random)(ab)
  }

  object Random {
    type RandomTuple[A] = Generator => (A, Generator)

    case class Generator(seed: Long) {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], Generator(seed2))
      }
    }

    def pure[A](a: A): RandomTuple[A] = {
      generator: Generator => (a, generator)
    }

    def map[A, B](tup: RandomTuple[A])(ab: A => B): RandomTuple[B] =
      flatMap(tup)(i => pure(ab(i)))

    def flatMap[A, B](tup: RandomTuple[A])(aFb: A => RandomTuple[B]): RandomTuple[B] = {
      generator: Generator => {
        val (a, gen) = tup(generator)
        aFb(a)(gen)
      }
    }

    def randomInt: RandomTuple[Int] = {
      generator: Generator => generator.nextInt
    }

    def randomPositiveInt: RandomTuple[Int] = for {
      n <- randomInt
    } yield n.abs

    def randomRange(min: Int, max: Int): RandomTuple[Int] = for {
      n <- randomPositiveInt
    } yield min + n % (max - min)

    def randomChar: RandomTuple[Char] = for {
      n <- randomRange(65, 90)
    } yield n.toChar

    def randomList(length: Int): RandomTuple[List[Int]] = for {
      a <- randomInt
      list <- if (length == 0) pure(Nil) else randomList(length - 1)
    } yield a :: list
  }
  val basicGenerator = Random.Generator(10101010)
  // println(Random.randomPositiveInt(basicGenerator))
  // println(Random.randomRange(2, 400)(basicGenerator))
  // println(Random.randomChar(basicGenerator))
  println(Random.randomList(5)(basicGenerator))
  // println(Random.randomInt.map(_.abs)(basicGenerator))
}
