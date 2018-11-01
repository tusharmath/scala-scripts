import scala.language.higherKinds
import scala.util.{Try}

object Main extends App {
  // Theory
  type Interpretter[A <: Msg] = A => Msg
  type Program[S]             = (Msg, S) => (Msg, S)

  // DSL
  trait AnswerTag
  object AnswerName     extends AnswerTag
  object AnswerGuess    extends AnswerTag
  object AnswerContinue extends AnswerTag

  sealed trait Msg
  case object Start                        extends Msg
  case object Noop                         extends Msg
  final case class Combine(a: Msg, b: Msg) extends Msg

  implicit class MsgSyntax(a: Msg) {
    def &&(b: Msg): Msg = Combine(a, b)
  }

  object Random {
    trait RandomMsg                        extends Msg
    case object RequestRandom              extends RandomMsg
    case class RandomDouble(value: Double) extends RandomMsg
    def double: RandomMsg = RequestRandom
    def interpret(msg: RandomMsg): Msg = {
      msg match {
        case RequestRandom => RandomDouble(Math.random)
      }
    }
  }

  object Console {
    // ADT
    trait ConsoleMsg                                 extends Msg
    case class Answer[T](result: String, tag: T)     extends ConsoleMsg
    case class Question[T](question: String, tag: T) extends ConsoleMsg
    case class Statement(message: String)            extends ConsoleMsg

    // Helpers
    def ask[T](question: String, tag: T) = Question(question, tag)
    def say(statement: String)           = Statement(statement)

    // Interpretter
    def interpret(msg: ConsoleMsg): Msg = {
      msg match {
        case Question(question, tag) => {
          println(s"${question}:")
          Answer(scala.io.StdIn.readLine(), tag)
        }
        case Statement(message) => println(message); Noop
      }
    }
  }

  def getIO[T <: Msg](msg: T): Msg = msg match {
    case m: Console.ConsoleMsg => Console.interpret(m)
    case m: Random.RandomMsg   => Random.interpret(m)
    case Combine(msgA, msgB)   => getIO(msgA) && getIO(msgB)
    case _                     => Noop
  }

  def run[S](program: Program[S])(msg: Msg, state: S)(
      getIO: Interpretter[Msg]): (Msg, S) = {

    val (nMsg, nState) = program(msg, state)
    val result         = getIO(nMsg)
    result match {
      case Noop => (Noop, nState)
      case Combine(msgA, msgB) => {
        val (rMsgA, rStateA) = run(program)(msgA, nState)(getIO)
        val (rMsgB, rStateB) = run(program)(msgB, rStateA)(getIO)
        (rMsgB, rStateB)
      }
      case _ => run(program)(result, nState)(getIO)
    }
  }

  // Application State
  case class ProgramState(guess: Option[Int] = None, random: Option[Int] = None)

  // The actual program
  def program(msg: Msg, state: ProgramState): (Msg, ProgramState) = {

    // Updates application state
    def update(msg: Msg, state: ProgramState): ProgramState = msg match {
      case Console.Answer(result, tag) =>
        tag match {
          case AnswerGuess => state.copy(guess = Try { result.toInt }.toOption)
          case _           => state
        }
      case _ => state
    }
    def isValidNumber(num: String): Boolean = {
      Try { num.toInt }.map(i => i >= 1 && i <= 5).getOrElse(false)
    }

    def invalidNumber = Console.say("Invalid number!")
    def guessANumber  = Console.ask("Guess a number (1-5)", AnswerGuess)
    def welcome(name: String) =
      Console.say(s"Welcome to the world of FP $name!")
    def askToContinue =
      Console.ask("Do you wish to continue (y/n)", AnswerContinue)
    def youWon = Console.say("You won!")
    def youLost(num: Int) =
      Console.say(s"Sorry you lost, the correct number was $num")
    def greet                    = Console.say("Greetings!")
    def getName                  = Console.ask("Enter Name", AnswerName)
    def dblToInt(double: Double) = (double * 5 + 1).toInt
    def checkGuess(state: ProgramState, double: Double): Boolean = {
      state.guess.getOrElse(-1) == dblToInt(double)
    }

    // Returns a side-effect causing msg
    def command(msg: Msg, state: ProgramState): Msg =
      msg match {
        case Start => greet && getName
        case Random.RandomDouble(value) => {
          if (checkGuess(state, value)) youWon
          else youLost(dblToInt(value))
        } && askToContinue
        case Console.Answer(result, tag) =>
          tag match {
            case AnswerName => welcome(result) && guessANumber
            case AnswerGuess => {
              if (isValidNumber(result)) Random.double
              else invalidNumber && guessANumber
            }
            case AnswerContinue => {
              if (result.toLowerCase == "y") guessANumber
              else Noop
            }
          }
        case _ => Noop
      }
    (command(msg, state), update(msg, state))
  }

  run(program)(Start, ProgramState())(getIO)
}
