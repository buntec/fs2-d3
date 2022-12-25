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
import d3.selection.namespace
import org.scalajs.dom

import concurrent.duration._

private[d3] trait TransitionManager[F[_]] {

  def next: F[Transition[F]]

  def run: F[Unit]

}

object TransitionManager {

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

          override def ease(
              node: dom.Element,
              ease: Double => Double
          ): F[Unit] = ts.update {
            _.updatedWith(id) {
              case None =>
                Some(
                  Map(
                    node -> Transition.Ts(
                      node,
                      ease = ease
                    )
                  )
                )
              case Some(m) =>
                Some(
                  m.updatedWith(node) {
                    case Some(ts) => Some(ts.copy(ease = ease))
                    case None =>
                      Some(
                        Transition.Ts(
                          node,
                          ease = ease
                        )
                      )
                  }
                )
            }
          }

          override def duration(
              node: dom.Element,
              duration: FiniteDuration
          ): F[Unit] = ts.update {
            _.updatedWith(id) {
              case None =>
                Some(
                  Map(
                    node -> Transition.Ts(
                      node,
                      duration = duration
                    )
                  )
                )
              case Some(m) =>
                Some(
                  m.updatedWith(node) {
                    case Some(ts) => Some(ts.copy(duration = duration))
                    case None =>
                      Some(
                        Transition.Ts(
                          node,
                          duration = duration
                        )
                      )
                  }
                )
            }
          }

          override def delay(
              node: dom.Element,
              delay: FiniteDuration
          ): F[Unit] = ts.update {
            _.updatedWith(id) {
              case None =>
                Some(
                  Map(
                    node -> Transition.Ts(
                      node,
                      delay = delay
                    )
                  )
                )
              case Some(m) =>
                Some(
                  m.updatedWith(node) {
                    case Some(ts) => Some(ts.copy(delay = delay))
                    case None =>
                      Some(
                        Transition.Ts(
                          node,
                          delay = delay
                        )
                      )
                  }
                )
            }
          }

          override def style(node: dom.Element, name: String, value: String)
              : F[Unit] =
            F.delay {
              node.asInstanceOf[dom.HTMLElement].style.getPropertyValue(name)
            }.flatMap { valueStart =>
              ts.update {
                _.updatedWith(id) {
                  case Some(m0) =>
                    Some(m0.updatedWith(node) {
                      case Some(ts) =>
                        Some(
                          ts.copy(
                            style = ts.style + (name -> Transition
                              .Style(name, valueStart, value))
                          )
                        )
                      case None =>
                        Some(
                          Transition.Ts(
                            node,
                            style = Map(
                              name -> Transition
                                .Style(name, valueStart, value)
                            )
                          )
                        )
                    })
                  case None =>
                    Some(
                      Map(
                        node -> Transition.Ts(
                          node,
                          style = Map(
                            name -> Transition
                              .Style(name, valueStart, value)
                          )
                        )
                      )
                    )
                }
              }
            }

          override def onComplete(cb: F[Unit]): F[Unit] =
            oc.update(xs => (id -> cb) :: xs)

          override def attr(
              node: dom.Element,
              name: String,
              value: String
          ): F[Unit] = {
            val fullname = namespace(name)
            F.delay {
              fullname match {
                case namespace.Name.Simple(name0) => node.getAttribute(name0)
                case namespace.Name.Namespaced(space, local) =>
                  node.getAttributeNS(space, local)
              }
            }.flatMap { valueStart =>
              ts.update {
                _.updatedWith(id) {
                  case Some(m0) =>
                    Some(m0.updatedWith(node) {
                      case Some(ts) =>
                        Some(
                          ts.copy(
                            attr = ts.attr + (fullname -> Transition
                              .Attr(fullname, valueStart, value))
                          )
                        )
                      case None =>
                        Some(
                          Transition.Ts(
                            node,
                            attr = Map(
                              fullname -> Transition
                                .Attr(fullname, valueStart, value)
                            )
                          )
                        )
                    })
                  case None =>
                    Some(
                      Map(
                        node -> Transition.Ts(
                          node,
                          attr = Map(
                            fullname -> Transition
                              .Attr(fullname, valueStart, value)
                          )
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
                    interpolate(attr.valueStart, attr.valueEnd)(
                      ts.ease(t)
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
                    interpolate(style.valueStart, style.valueEnd)(
                      ts.ease(t)
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
