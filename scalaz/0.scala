// http://eed3si9n.com/learning-scalaz/index.html

import scala.language.higherKinds

// DAY 0

/// --- MONOID ---
trait Monoid[T] {
  def mZero: T
  def mAppend(a: T, b: T): T
}

object Monoid {
  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def mZero: Int = 0
    def mAppend(a: Int, b: Int): Int = a + b
  }

  implicit val stringMonoid: Monoid[String] = new Monoid[String] {
    def mZero: String = ""
    def mAppend(a: String, b: String): String = a + b
  }
}

/// --- FOLD ---

trait FoldLeft[F[_]] {
  def foldLeft[A: Monoid](a: F[A]): A
}

object FoldLeft {
  implicit val foldList: FoldLeft[List] = new FoldLeft[List] {
    def foldLeft[T: Monoid](t: List[T]): T = {
      val m = implicitly[Monoid[T]]
      t.foldLeft(m.mZero)(m.mAppend)
    }
  }
}

object Main extends App {
  def sumList[A: Monoid](list: List[A]): A = {
    val m = implicitly[Monoid[A]]
    list.foldLeft(m.mZero)(m.mAppend)
  }

  def sum[A: Monoid, F[_]: FoldLeft](list: F[A]): A = {
    implicitly[FoldLeft[F]].foldLeft(list)
  }

  println(sumList(List(1, 2, 3)))
  println(sumList(List("1", "2", "3")))
}
