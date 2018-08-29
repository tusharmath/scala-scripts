import scala.language.higherKinds

object REPL extends App {
  // Create a random number generator class
  case class RandomNumberGenerator(seed: Long) {
    def nextInt() = {
      val seed2 = (seed*0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
      ((seed2 >>> 16).asInstanceOf[Int], RandomNumberGenerator(seed2))
    }
  }
}
