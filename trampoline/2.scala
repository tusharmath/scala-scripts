/**
  https://medium.com/@olxc/trampolining-and-stack-safety-in-scala-d8e86474ddfa
 */

import scala.language.higherKinds


object Main extends App {
  sealed trait Trampoline[A]
  case class Done[A](value: A) extends Trampoline[A]
  case class More[A](call: () => Trampoline[A]) extends Trampoline[A]

  def even[A](l: List[A]): Trampoline[Boolean] = l match {
    case Nil => Done(true)
    case x :: xs => More(() => odd(xs))
  }

  def odd[A](l: List[A]): Trampoline[Boolean] = l match {
    case Nil => Done(false)
    case x :: xs => More(() => even(xs))
  }

  def run[A](trampoline: Trampoline[A]): A = trampoline match {
    case Done(v) => v
    case More(t) => run(t()) // <- tail recursive, yay
  }

  println(run(even((0 to 100000 - 1).toList)))
}