import scala.language.higherKinds

object Main extends App {
  def even[A](l: List[A]): Boolean = l match {
    case Nil => true
    case x :: xs => odd(xs)
  }
  def odd[A](l: List[A]): Boolean = l match {
    case Nil => false
    case x :: xs => even(xs)
  }

  even((0 to 10000).toList) // blows the stack
}