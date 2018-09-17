object Main extends App {
  def app0 = {
    // --- EQUAL ---
    implicit class EqualSyntax[T](a: T) {
      def ===(b: T): Boolean = a.equals(b)
      def =/=(b: T): Boolean = !a.equals(b)
    }
    implicit class ToOptionalSyntax[T](t: T) {
      def some = Some(t)
    }

    println(1 === 1) // true
    println(Some(1).equals(Some(2))) // false
    println(Some(1) === Some(2)) // false
    println(Some(1) === Some(1)) // true
    println(Some(1) =/= Some(1)) // false
    println(1.some === 2.some) // false

    val a = (i: Int) => i + 1
    val b = (i: Int) => i + 1
    println(a === a) // works!
  }

  def app1 = {
    // --- ORDER ---
    println("ORDER")
    trait Ordering
    case object LT extends Ordering
    case object GT extends Ordering
    case object EQ extends Ordering

    case class Compare[T](compare: (T, T) => Int)

    implicit val compareInt = new Compare((a: Int, b: Int) => a - b)
    implicit val compareDouble = new Compare(
      (a: Double, b: Double) => if (a > b) 1 else if (b > a) -1 else 0)
    implicit val compareString = new Compare(
      (a: String, b: String) => a.compareTo(b))
    implicit class orderingSyntax[T: Compare](a: T) {
      private def O = implicitly[Compare[T]]
      def ?|?(b: T): Ordering = {
        if (O.compare(a, b) > 0) GT
        else if (O.compare(a, b) < 0) LT
        else EQ
      }
      def max(b: T): T = a ?|? b match {
        case EQ => a
        case GT => b
        case LT => a
      }
    }
    println(1.0 ?|? 2.0) // LT
    println(1 ?|? 2) // LT
    println("a" ?|? "b") // LT
    println("b" ?|? "a") // GT
    println(1 max 2 max 10) // 10
  }

  app1
}
