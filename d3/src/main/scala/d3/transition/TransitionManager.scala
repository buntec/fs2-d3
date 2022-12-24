/*
 * Copyright 2022 buntec
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package d3.transition

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.kernel.Resource
import cats.syntax.all._
import d3.internal.Scheduler
import org.scalajs.dom

import concurrent.duration._
import d3.selection.namespace

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

object Transition {

  case class Attr(
      name: namespace.Name,
      valueStart: String,
      valueEnd: String
  )

  case class Style(name: String, valueStart: String, valueEnd: String)

  case class Ts(
      node: dom.Element,
      duration: FiniteDuration,
      delay: FiniteDuration,
      attr: Map[namespace.Name, Attr],
      style: Map[String, Style]
  )

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
      ts <- F
        .ref(Map.empty[Long, Map[dom.Element, Transition.Ts]])
        .toResource
      oc <- F.ref(List.empty[(Long, F[Unit])]).toResource
      id <- F.ref(0L).toResource
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
            val fullname = namespace(name)
            F.delay {
              fullname match {
                case namespace.Name.Simple(name0) => node.getAttribute(name0)
                case namespace.Name.Namespaced(space, local) =>
                  node.getAttributeNS(space, local)
              }
            }.flatMap { valueStart =>
              ts.update { m =>
                m.updatedWith(id) {
                  case Some(m0) =>
                    Some(m0.updatedWith(node) {
                      case Some(ts) =>
                        Some(
                          ts.copy(
                            duration = duration,
                            delay = delay,
                            attr = ts.attr + (fullname -> Transition
                              .Attr(fullname, valueStart, value))
                          )
                        )
                      case None =>
                        Some(
                          Transition.Ts(
                            node,
                            duration,
                            delay,
                            Map(
                              fullname -> Transition
                                .Attr(fullname, valueStart, value)
                            ),
                            Map.empty
                          )
                        )
                    })
                  case None =>
                    Some(
                      Map(
                        node -> Transition.Ts(
                          node,
                          duration,
                          delay,
                          Map(
                            fullname -> Transition
                              .Attr(fullname, valueStart, value)
                          ),
                          Map.empty
                        )
                      )
                    )
                }
              }
            }
          }
        }
      }

      private def run(m: Map[dom.Element, Transition.Ts]): F[Unit] = {
        val totalDuration = m.map { case (_, ts) => ts.duration + ts.delay }.max
        Scheduler.awakeEveryAnimationFrame
          .takeThrough(_ < totalDuration)
          .evalMap { elapsed =>
            m.toList.traverse_ { case (elm, ts) =>
              F.whenA(elapsed >= ts.delay) {
                val t = math
                  .min(
                    1.0,
                    (elapsed - ts.delay).toMillis.toDouble / ts.duration.toMillis
                  )

                val setAttr = ts.attr.toList.traverse_ { case (name, attr) =>
                  val interpolatedValue =
                    interpolator(attr.valueStart, attr.valueEnd)(
                      d3.ease.cubicInOut(t)
                    )
                  F.delay(
                    name match {
                      case namespace.Name.Simple(name) =>
                        elm.setAttribute(name, interpolatedValue)
                      case namespace.Name.Namespaced(space, local) =>
                        elm.setAttributeNS(space, local, interpolatedValue)
                    }
                  )
                }

                val setStyle = ts.style.toList.traverse_ { case (name, style) =>
                  val interpolatedValue =
                    interpolator(style.valueStart, style.valueEnd)(
                      d3.ease.cubicInOut(t)
                    )
                  F.delay(
                    elm
                      .asInstanceOf[dom.HTMLElement]
                      .style
                      .setProperty(name, interpolatedValue, "")
                  )
                }

                setAttr >> setStyle
              }
            }
          }
          .compile
          .drain
      }

      override def run: F[Unit] = oc.get.flatMap { oc =>
        val ocm = oc.groupBy(_._1).map { case (k, v) => k -> v.map(_._2) }
        ts.get.flatMap { m =>
          m.toList.sortBy(_._1).traverse_ { case (id, ts) =>
            run(ts) >> ocm.get(id).fold(F.unit)(_.parSequence_)
          }
        }
      }

    }

}
