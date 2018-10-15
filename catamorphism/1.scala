object Main extends App {
  println("YO!")
  sealed trait Expression
  case class Value(v: Int) extends Expression
  case class Add(e1: Expression, e2: Expression) extends Expression
  case class Mult(e1: Expression, e2: Expression) extends Expression

  // Expression evaluator
  def eval(exp: Expression): Int = exp match {
    case Value(v)   => v
    case Add(a, b)  => eval(a) + eval(b)
    case Mult(a, b) => eval(a) * eval(b)
  }

  val expr = Mult(Add(Value(1), Value(2)), Value(3))
  println(expr)
  println(eval(expr))
}
