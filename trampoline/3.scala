import scala.language.higherKinds


object Main extends App {
  sealed trait Trampoline[A] {
    def resume: Either[() => Trampoline[A], A] = this match {
      case Done(v) => Right(v)
      case More(k) => Left(k)
    }

    final def runT: A = resume match {
      case Right(value) => value
      case Left(more) => more().runT
    }
  }
  case class Done[A](value: A) extends Trampoline[A]
  case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

  def app() = {
    def even[A](l: List[A]): Trampoline[Boolean] = l match {
      case Nil => Done(true)
      case x :: xs => More(() => odd(xs))
    }

    def odd[A](l: List[A]): Trampoline[Boolean] = l match {
      case Nil => Done(false)
      case x :: xs => More(() => even(xs))
    }
    even((0 to 1000000).toList)
  }

  println(app().runT)
}