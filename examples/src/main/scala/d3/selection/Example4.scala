package d3.selection.examples

import cats.syntax.all._
import cats.effect.kernel.Async
import org.scalajs.dom
import concurrent.duration._

import fs2.Stream

import d3.internal.Scheduler

class Example4[F[_]](implicit F: Async[F]) {

  def run2: F[Unit] = Stream
    .awakeEvery(100.millis, true)
    .evalMap { d =>
      F.sleep(500.millis) >> F.delay(println(s"elapsed=${d.toMillis} ms"))
    }
    .interruptAfter(3.second)
    .compile
    .drain

  def run: F[Unit] = Scheduler.awakeEveryAnimationFrame
    .evalMap { d =>
      F.delay(println(s"elapsed=${d.toMillis} ms"))
    }
    .interruptAfter(3.second)
    .compile
    .drain

}
