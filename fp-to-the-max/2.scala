import scala.io.StdIn._
import scala.util._

object Intro {

}

object App2 {
  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
  def main(args: Array[String]) = {
    println("Whats your name?")
    val name = readLine()
    println("Hello, " +  name + ", welcome to the game!")
    var exec = true
    while(exec) {
      val num = scala.util.Random.nextInt(5)
      println("Dear " + name + ", please guess a number from 1 to 5:")

      val guess = parseInt(readLine())
      guess match {
        case None => println("You did not enter a number")
        case Some(guess) =>
          if (guess == num) println("You guess right, " + name + "!")
          else println("You guessed wrong, " + name + "! The number was: " + num)
      }

      println("Do you want to continue, " + name + "?")
      var cont = true
      while(cont) {
        cont = false
        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _ => {
            println("You entered an invalid option, select y/n:")
            cont = true
          }
        }
      }

    }
  }
}

