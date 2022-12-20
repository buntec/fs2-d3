package d3.selection.examples

import cats.effect.IO
import cats.effect.IOApp

object Main extends IOApp.Simple {

  override def run: IO[Unit] = new Example2[IO].run

}
