import scala.language.higherKinds
import scala.util.{Try}

object Main extends App {

  // DSL
  trait AnswerTag
  object AnswerName     extends AnswerTag
  object AnswerGuess    extends AnswerTag
  object AnswerContinue extends AnswerTag

  sealed trait Msg
  case object Start                                     extends Msg
  case object Noop                                      extends Msg
  case class Statement(message: String)                 extends Msg
  case class Question(question: String, tag: AnswerTag) extends Msg
  case class Answer(result: String, tag: AnswerTag)     extends Msg
  case object RequestRandom                             extends Msg
  case class RandomResponse(value: Double)              extends Msg
  final case class Combine(a: Msg, b: Msg)              extends Msg

  implicit class MsgSyntax(a: Msg) {
    def &&(b: Msg): Msg = Combine(a, b)
  }

  // ADT: Console

  // Application State
  case class ProgramState(guess: Option[Int] = None, random: Option[Int] = None)

  // The actual program
  def program(msg: Msg, state: ProgramState): (Msg, ProgramState) = {

    // Updates application state
    def update(msg: Msg, state: ProgramState): ProgramState = msg match {
      case Answer(result, tag) =>
        tag match {
          case AnswerGuess => state.copy(guess = Try { result.toInt }.toOption)
          case _           => state
        }
      case _ => state
    }
    def isValidNumber(num: String): Boolean = {
      Try { num.toInt }.map(i => i >= 1 && i <= 5).getOrElse(false)
    }

    def invalidNumber         = Statement("Invalid number!")
    def guessANumber          = Question("Guess a number (1-5)", AnswerGuess)
    def welcome(name: String) = Statement(s"Welcome to the world of FP $name!")
    def askToContinue =
      Question("Do you wish to continue (y/n)", AnswerContinue)
    def youWon = Statement("You won!")
    def youLost(num: Int) =
      Statement(s"Sorry you lost, the correct number was $num")
    def greet                    = Statement("Greetings!")
    def getName                  = Question("Enter Name", AnswerName)
    def dblToInt(double: Double) = (double * 5 + 1).toInt
    def checkGuess(state: ProgramState, double: Double): Boolean = {
      state.guess.getOrElse(-1) == dblToInt(double)
    }

    // Returns a side-effect causing msg
    def command(msg: Msg, state: ProgramState): Msg =
      msg match {
        case Start => greet && getName
        case RandomResponse(double) => {
          if (checkGuess(state, double)) youWon
          else youLost(dblToInt(double))
        } && askToContinue
        case Answer(result, tag) =>
          tag match {
            case AnswerName => welcome(result) && guessANumber
            case AnswerGuess => {
              if (isValidNumber(result)) RequestRandom
              else invalidNumber && guessANumber
            }
            case AnswerContinue => {
              if(result.toLowerCase == "y") guessANumber
              else Noop
            }
          }
        case _ => Noop
      }
    (command(msg, state), update(msg, state))
  }

  type Program[S] = (Msg, S) => (Msg, S)

  def getIO(msg: Msg): Msg = msg match {
    case Question(question, tag) => {
      println(s"${question}:")
      Answer(scala.io.StdIn.readLine(), tag)
    }
    case Statement(message)  => println(message); Noop
    case Combine(msgA, msgB) => getIO(msgA) && getIO(msgB)
    case RequestRandom       => RandomResponse(Math.random)
    case _                   => Noop
  }

  def run[S](program: Program[S])(msg: Msg, state: S): (Msg, S) = {

    val (nMsg, nState) = program(msg, state)

    val result = getIO(nMsg)
    result match {
      case Noop => (Noop, nState)
      case Combine(msgA, msgB) => {
        val (rMsgA, rStateA) = run(program)(msgA, nState)
        val (rMsgB, rStateB) = run(program)(msgB, rStateA)
        (rMsgB, rStateB)
      }
      case _ => run(program)(result, nState)
    }
  }

  run(program)(Start, ProgramState())
}
