import java.io.IOException

import zio.{App, UIO, ZIO}
import zio.console._

object Hello extends App {
  def run(args: List[String]): ZIO[Console, Nothing, Int] =
    helloApp.fold(_ => 1, _ => 0)


  val helloApp: ZIO[Console, IOException, Unit] =
    for {
      _    <- putStrLn("Hello! Can I know your name?")
      name <- getStrLn
      _    <- putStrLn(s"Hello $name, have a good day!")
    } yield ()

}
