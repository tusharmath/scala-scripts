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

    def randomInt: RandomTuple[Int] = {
      generator: Generator => generator.nextInt
    }

    def randomPositiveInt: RandomTuple[Int] = {
      generator: Generator => {
        val (a, gen) = randomInt(generator)
        (a.abs, gen)
      }
    }

    def randomRange(min: Int, max: Int): RandomTuple[Int] = (generator: Generator) => {
      val (a, gen) = randomPositiveInt(generator)
      val b = min + a % (max - min)
      (b, gen)
    }

    def randomChar: RandomTuple[Char] = (generator: Generator) => {
      val (a, gen) = randomRange(65, 90)(generator)
      (a.toChar, gen)
    }

    def randomSequence(length: Int): RandomTuple[List[Int]] = (generator: Generator) => {
      val range = 0 to length
      val numbers = range
        .foldLeft(List(basicGenerator))((a, b) => a.head.nextInt._2 :: a)
        .map(_.nextInt)
      (numbers.map(_._1), numbers.head._2)
    }
  }
  val basicGenerator = Random.Generator(10101010)
  println(Random.randomPositiveInt(basicGenerator))
}
