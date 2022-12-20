package d3.transition

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._
import d3.internal.Scheduler

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

  def onComplete(cb: F[Unit]): F[Unit]

}

object TransitionManager {

  def interpolator(value0: String, value1: String) = {
    if (value0 == null) { (t: Double) =>
      if (t < 1) value0 else value1
    } else {
      (value0.toDoubleOption, value1.toDoubleOption).tupled match {
        case None           => (t: Double) => if (t < 1) value0 else value1
        case Some((x0, x1)) => (t: Double) => (t * x1 + (1.0 - t) * x0).toString
      }
    }
  }

  def apply[F[_]](implicit F: Async[F]): Resource[F, TransitionManager[F]] =
    for {

      ts <- F.ref(List.empty[(Int, F[Unit])]).toResource
      oc <- F.ref(List.empty[(Int, F[Unit])]).toResource
      id <- F.ref(0).toResource

    } yield new TransitionManager[F] {

      override def next: F[Transition[F]] = id.getAndUpdate(_ + 1).map { id =>
        new Transition[F] {

          override def onComplete(cb: F[Unit]): F[Unit] =
            oc.update(xs => (id -> cb) :: xs)

          override def attr(
              node: dom.Element,
              name: String,
              value: String,
              duration: FiniteDuration,
              delay: FiniteDuration
          ): F[Unit] = {
            val task = F.delay(node.getAttribute(name)).flatMap { value0 =>
              val interp = interpolator(value0, value)
              F.sleep(delay) >>
                Scheduler.awakeEveryAnimationFrame
                  .takeThrough(_ < duration)
                  .evalMap { elapsed =>
                    val t = math
                      .min(1.0, elapsed.toMillis.toDouble / duration.toMillis)
                    val interpolatedValue = interp(d3.ease.cubicInOut(t))
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
        oc.get.flatMap { onCompletes =>
          val ocById =
            onCompletes.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
          ts.get.flatMap { tasks =>
            tasks.groupBy(_._1).toList.sortBy(_._1).traverse_ {
              case (id, tasks) =>
                tasks
                  .map(_._2)
                  .parSequence_ *> ocById.get(id).fold(F.unit)(_.parSequence_)
            }
          }
        }

    }

}
