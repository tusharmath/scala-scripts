import scala.language.higherKinds

object Main extends App {
  trait FlatMap[F[_]] {
    def flatMap[A, B](fa: F[A])(aFb: A => F[B]): F[B]
  }

  implicit val optionFlatMap = new FlatMap[Option] {
    def flatMap[A, B](fa: Option[A])(aFb: A => Option[B]): Option[B] = {
      fa.flatMap(aFb)
    }
  }

  case class Kleisli[F[_]: FlatMap, B, C](run: B => F[C]) {
    private def F = implicitly[FlatMap[F]]
    def +[A](b: Kleisli[F, A, B]): Kleisli[F, A, C] = {
      Kleisli(a => F.flatMap(b.run(a))(run))
    }
    def apply(b: B): F[C] = run(b)
  }

  // can not parse.andThen(reciprocal)

  val parseK = Kleisli[Option, String, Int](
    i => if (i.matches("-?[0-9]+")) Some(i.toInt) else None
  )

  val reciprocalK = Kleisli[Option, Int, Double](
    i => if (i != 0) Some(1.0 / i) else None
  )

  println("Yay!")

  val parseNreciprocate = reciprocalK + parseK
  println(parseNreciprocate("0"))
}
