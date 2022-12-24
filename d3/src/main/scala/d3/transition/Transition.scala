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

import d3.selection.namespace
import org.scalajs.dom

import concurrent.duration._

private[d3] trait Transition[F[_]] {

  def attr(
      node: dom.Element,
      name: String,
      value: String
  ): F[Unit]

  def delay(node: dom.Element, delay: FiniteDuration): F[Unit]

  def duration(node: dom.Element, duration: FiniteDuration): F[Unit]

  def onComplete(cb: F[Unit]): F[Unit]

  def style(
      node: dom.Element,
      name: String,
      value: String
  ): F[Unit]

}

private[d3] object Transition {

  case class Attr(
      name: namespace.Name,
      valueStart: String,
      valueEnd: String
  )

  case class Style(name: String, valueStart: String, valueEnd: String)

  case class Ts(
      node: dom.Element,
      duration: FiniteDuration = 750.millis,
      delay: FiniteDuration = 0.millis,
      attr: Map[namespace.Name, Attr] = Map.empty,
      style: Map[String, Style] = Map.empty
  )

}
