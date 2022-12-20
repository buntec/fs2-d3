package d3.selection.examples

import cats.effect.kernel.Async
import cats.syntax.all._
import d3.internal.Scheduler

import concurrent.duration._

class Example4[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = Scheduler.awakeEveryAnimationFrame
    .evalMap { d =>
      F.delay(println(s"elapsed=${d.toMillis} ms"))
    }
    .interruptAfter(1.second)
    .compile
    .drain

}
