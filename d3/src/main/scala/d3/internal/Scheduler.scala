package d3.internal

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.Queue
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import scala.concurrent.duration.Duration

object Scheduler {

  def awakeEveryAnimationFrame[F[_]](implicit
      F: Async[F]
  ): Stream[F, Duration] = {
    val queue = (
      Dispatcher.sequential,
      Resource.make(F.ref(true))(_.set(false)), // perhaps this is not needed?
      Resource.make(F.ref(Option.empty[Int]))(_.get.flatMap {
        case None     => F.unit
        case Some(id) => F.delay(dom.window.cancelAnimationFrame(id))
      }),
      Queue.dropping[F, Double](1).toResource
    ).tupled.flatMap { case (dispatcher, signal, id, queue) =>
      def go: F[Unit] = signal.get.flatMap {
        case true =>
          F.bracket(F.delay(dom.window.requestAnimationFrame { t =>
            dispatcher.unsafeRunAndForget(queue.offer(t) *> go)
          }))(_ => F.unit)(id0 => id.set(Some(id0)))
        case false => F.unit
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
