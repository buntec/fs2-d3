package d3.internal

import org.scalajs.dom

import fs2.Stream
import cats.syntax.all._
import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import fs2.concurrent.SignallingRef
import cats.effect.std.Queue
import cats.effect.kernel.Resource
import scala.concurrent.duration.Duration

object Scheduler {

  def awakeEveryAnimationFrame[F[_]](implicit
      F: Async[F]
  ): Stream[F, Duration] = {
    val queue = (
      Dispatcher.sequential,
      Resource.make(F.ref(false))(_.set(true)),
      Resource.make(F.ref(Option.empty[Int]))(_.get.flatMap {
        case None     => F.unit
        case Some(id) => F.delay(dom.window.cancelAnimationFrame(id))
      }),
      Queue.dropping[F, Double](1).toResource
    ).tupled.flatMap { case (dispatcher, signal, id, queue) =>
      def go: F[Unit] = signal.get.flatMap {
        case true => F.unit
        case false =>
          F.delay(dom.window.requestAnimationFrame { t =>
            dispatcher.unsafeRunAndForget(queue.offer(t) *> go)
          }).flatMap(id0 => id.set(Some(id0)))
      }

      (go >> F.never[Unit]).background >> Resource.pure(queue)
    }

    Stream.eval(F.monotonic).flatMap { start =>
      Stream.resource(queue).flatMap { queue =>
        Stream.fromQueueUnterminated(queue)
      } >> Stream.eval(
        F.monotonic.map(_ - start)
      )
    }

  }

}
