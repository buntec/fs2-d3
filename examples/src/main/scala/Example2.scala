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

package examples

import cats.effect.kernel.Async
import cats.syntax.all._
import org.scalajs.dom

class Example2[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = {
    val w = 500
    val h = 500
    val margin = 50

    val setup = for {
      svg <- d3
        .select("#app")
        .append("svg")
        .attr("width", s"${w + 2 * margin}".some)
        .attr("height", s"${h + 2 * margin}".some)
        .compile
        .nodeOrError[F, dom.Element]
      g <- d3
        .select(svg)
        .append("g")
        .attr("transform", s"translate($margin, $margin)".some)
        .compile
        .nodeOrError[F, dom.Element]
    } yield g

    setup.flatMap { g =>
      val x = d3.scale.ScaleContinuous.linear((0, 100.0), (0, w.toDouble))
      val y = d3.scale.ScaleContinuous.linear((0, 100.0), (0, h.toDouble))

      val xAxis = d3.axis.axisTop(x)
      val makeXAxis = xAxis(d3.select[F, dom.Element, Nothing](g).append("g"))

      val yAxis = d3.axis.axisLeft(y)
      val makeYAxis = yAxis(d3.select[F, dom.Element, Nothing](g).append("g"))

      (makeXAxis, makeYAxis).tupled.void
    }
  }

}
