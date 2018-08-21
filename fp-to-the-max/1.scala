import scala.io.StdIn._

object Main extends App {
  def main(args: Array[String]) = {
    println("Whats your name?")
    val name = readLine()
    println("Hello, " +  name + ", welcome to the game!")
    var exec = true
    while(exec) {
      val num = scala.util.Random.nextInt(5)
      println("Dear " + name + ", please guess a number from 1 to 5:")

      val guess = readLine().toInt
      if (guess == num) println("You guess right, " + name + "!")
      else println("You guessed wrong, " + name + "! The number was: " + num)

      println("Do you want to continue, " + name + "?")
      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }
    }
  }
}
