object Main extends App {

  trait Lens[T, A] {
    def get(t: T): A
    def set(a: A)(t: T): T
    def mod(aa: A => A): T => T = t => set(aa(get(t)))(t)
  }

  implicit class LensSyntax[A, B](ab: Lens[A, B]) {
    def :::[C](bc: Lens[B, C]) = Lens.compose(ab, bc)
  }

  object Lens {
    def lift[T, A](getter: T => A, setter: (T, A) => T): Lens[T, A] =
      new Lens[T, A] {
        def get(t: T): A = getter(t)
        def set(a: A)(t: T): T = setter(t, a)
      }

    def compose[A, B, C](ab: Lens[A, B], bc: Lens[B, C]): Lens[A, C] =
      lift[A, C](
        (a: A) => bc.get(ab.get(a)),
        (a: A, c: C) => ab.set(bc.set(c)(ab.get(a)))(a)
      )
  }

  case class Size(value: Int)
  case class Position(x: Int, y: Int)
  case class Turtle(size: Size, position: Position)
  object Turtle {
    val xPositionLens = {
      Lens.lift[Position, Int](_.x, (t, a) => t.copy(x = a))
    }
    val yPositionLens = {
      Lens.lift[Position, Int](_.y, (t, a) => t.copy(y = a))
    }
    val positionXYLens = {
      Lens.lift[Position, (Int, Int)](
        p => (p.x, p.y),
        (t, a) => Position(a._1, a._2)
      )
    }
    val sizeLens = {
      Lens.lift[Size, Int](_.value, (t, a) => t.copy(value = a))
    }
    val turtleSizeLens = {
      Lens.lift[Turtle, Size](_.size, (t, a) => t.copy(size = a))
    }
    val turtlePositionLens = {
      Lens.lift[Turtle, Position](_.position, (t, a) => t.copy(position = a))
    }

    val turtleXLens = { positionXYLens ::: turtlePositionLens }

    def left = turtleXLens.mod(_ match { case (x, y) => (x - 1, y) })

  }
  val turtle0 = Turtle(
    size = Size(10),
    position = Position(10, 20)
  )

  println(Turtle.left(turtle0))
}
