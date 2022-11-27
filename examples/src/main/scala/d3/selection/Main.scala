package d3.selection.examples

import cats.effect.{IO, IOApp}

object Main extends IOApp.Simple {

  override def run: IO[Unit] = new Example1[IO].run

}
