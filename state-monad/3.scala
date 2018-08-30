import scala.language.higherKinds

object REPL extends App {
  object Random {
    type RandomState[A] = (A, Generator)
    case class Generator(seed: Long) {
      def nextInt: RandomState[Int] = {
        val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
        ((seed2 >>> 16).asInstanceOf[Int], Generator(seed2))
      }
    }

    def randomInt(generator: Generator): RandomState[Int] = {
      generator.nextInt
    }

    def randomPositiveInt(generator: Generator): RandomState[Int] = {
      val (a, gen) = generator.nextInt
      (a.abs, gen)
    }

    def randomRange(generator: Generator)(min: Int, max: Int): RandomState[Int] = {
      val (a, gen) = randomPositiveInt(generator)
      val b = min + a % (max - min)
      (b, gen)
    }

    def randomChar(generator: Generator): RandomState[Char] = {
      val (a, gen) = randomRange(generator)(65, 90)
      (a.toChar, gen)
    }
    def randomSequence(generator: Generator)(length: Int): RandomState[List[Int]] = {
      val range = 0 to length
      val numbers = range
        .foldLeft(List(basicGenerator))((a, b) => a.head.nextInt._2 :: a)
        .map(_.nextInt)
      (numbers.map(_._1), numbers.head._2)
    }
  }
  val basicGenerator = Random.Generator(10101010)
  println(Random.randomSequence(basicGenerator)(3))
}
