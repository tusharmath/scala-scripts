object Mango {
  case class IO[A](unsafeRun: () => A)

  def run(): IO[Unit] = {

    val D = IO(() => "Hi there")

    IO[Unit](() => println("Keek"))

  }
}
