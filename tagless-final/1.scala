/*
 * https://blog.scalac.io/exploring-tagless-final.html
 * https://www.youtube.com/watch?v=IhVdU4Xiz2U
 */


import scala.language.higherKinds

object Main extends App {
  // Language (DSL for the dev)
  trait Language[Wrapper[_]] {
    def number(v: Int): Wrapper[Int]
    def increment(a: Wrapper[Int]): Wrapper[Int]
    def add(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]
    // def multiply(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]

    def text(v: String): Wrapper[String]
    def toUpper(a: Wrapper[String]): Wrapper[String]
    def concat(a: Wrapper[String], b: Wrapper[String]): Wrapper[String]

    def toString(v: Wrapper[Int]): Wrapper[String]
  }

  // trait LanguageWithMul[Wrapper[_]] extends Language[Wrapper] {
  //   def multiply(a: Wrapper[Int], b: Wrapper[Int]): Wrapper[Int]
  // }

  // Bridges (Business logic)
  trait ScalaToLanguageBridge[ScalaValue] {
    def apply[Wrapper[_]](L: Language[Wrapper]): Wrapper[ScalaValue]
  }

  def buildComplexExpression(text: String, a: Int, b: Int) = new ScalaToLanguageBridge[String] {
    override def apply[Wrapper[_]](F: Language[Wrapper]): Wrapper[String] = {
      val addition = F.add(F.number(a), F.increment(F.number(b)))
      F.concat(F.text(text), F.toString(addition))
    }
  }

  // Interpreters (Runs logic)
  type NoWrap[ScalaValue] = ScalaValue

  val interpret = new Language[NoWrap] {
    override def number(v: Int): NoWrap[Int] = v
    override def increment(a: NoWrap[Int]): NoWrap[Int] = a + 1
    override def add(a: NoWrap[Int], b: NoWrap[Int]): NoWrap[Int] = a + b

    override def text(v: String): NoWrap[String] = v
    override def toUpper(a: NoWrap[String]): NoWrap[String] = a.toUpperCase
    override def concat(a: NoWrap[String], b: NoWrap[String]): NoWrap[String] = a + " " + b

    override def toString(v: NoWrap[Int]): NoWrap[String] = v.toString
  }


  // bootstrap
  val fullExpression = buildComplexExpression("Result is ", 110, 1)
  println(s"interpreted full: ${fullExpression.apply(interpret)}")

}