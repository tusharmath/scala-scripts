import scala.language.higherKinds

object REPL extends App {
  object Random {
    type RandomTuple[A] = Generator => (A, Generator)
    case class Generator(seed: Long) {
      def nextInt = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], Generator(seed2))
      }
    }

    def map[A, B](tup: RandomTuple[A])(ab: A => B): RandomTuple[B] = {
      generator: Generator => {
        val (a, gen) = tup(generator)
        (ab(a), gen)
      }
    }

    def randomInt: RandomTuple[Int] = {
      generator: Generator => generator.nextInt
    }

    def randomPositiveInt: RandomTuple[Int] = map(randomInt)(_.abs)
    def randomRange(min: Int, max: Int): RandomTuple[Int] = map(randomPositiveInt)(min + _ % (max - min))
    def randomChar: RandomTuple[Char] = map(randomRange(65, 90))(_.toChar)
    def randomSequence(length: Int): RandomTuple[List[Int]] = (generator: Generator) => {
      val range = 0 to length
      val numbers = range
        .foldLeft(List(generator))((a, b) => a.head.nextInt._2 :: a)
        .map(_.nextInt)
      (numbers.map(_._1), numbers.head._2)
    }
  }
  val basicGenerator = Random.Generator(10101010)
  println(Random.randomPositiveInt(basicGenerator))
  println(Random.randomRange(2, 400)(basicGenerator))
  println(Random.randomChar(basicGenerator))
  println(Random.randomSequence(5)(basicGenerator))
}
