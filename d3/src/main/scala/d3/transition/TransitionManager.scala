package d3.transition

import org.scalajs.dom
import cats.syntax.all._
import cats.effect.implicits._
import cats.effect.kernel.Resource
import cats.effect.kernel.Async
import fs2.Stream
import concurrent.duration._

trait TransitionManager[F[_]] {

  def next: F[Transition[F]]

  def run: F[Unit]

}

trait Transition[F[_]] {

  def attr(
      node: dom.Element,
      key: String,
      value: String,
      duration: FiniteDuration,
      delay: FiniteDuration
  ): F[Unit]

}

object TransitionManager {

  def apply[F[_]](implicit F: Async[F]): Resource[F, TransitionManager[F]] =
    for {

      ts <- F.ref(List.empty[(Int, F[Unit])]).toResource
      id <- F.ref(0).toResource

    } yield new TransitionManager[F] {

      override def next: F[Transition[F]] = id.getAndUpdate(_ + 1).map { id =>
        new Transition[F] {

          override def attr(
              node: dom.Element,
              name: String,
              value: String,
              duration: FiniteDuration,
              delay: FiniteDuration
          ): F[Unit] = {
            val task = F.delay(node.getAttribute(name)).flatMap { value0 =>
              F.sleep(delay) >>
                Stream
                  .awakeEvery(16.millis)
                  .takeThrough(_ < duration)
                  .evalMap { elapsed =>
                    val t =
                      math
                        .min(1.0, elapsed.toMillis.toDouble / duration.toMillis)
                    val interpolatedValue =
                      "foo" // interpolation(value0, value, t)
                    F.delay(
                      node.setAttribute(name, interpolatedValue)
                    )
                  }
                  .compile
                  .drain
            }

            ts.update(tasks => (id, task) :: tasks)

          }

        }
      }

      override def run: F[Unit] =
        ts.get.flatMap { tasks =>
          tasks.groupBy(_._1).toList.sortBy(_._1).traverse_ { case (_, tasks) =>
            tasks.map(_._2).parSequence_
          }
        }

    }

}
