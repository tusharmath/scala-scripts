object Main extends App {
  def parse(s: String): Option[Int] =
    if (s.matches("-?[0-9]+")) Some(s.toInt) else None

  def reciprocal(i: Int): Option[Double] =
    if (i != 0) Some(1.0 / i) else None

  // can not parse.andThen(reciprocal)
  parse("100")
    .flatMap(reciprocal)
    .foreach(println)

}
