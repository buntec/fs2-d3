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
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example2[F[_]](implicit F: Async[F]) {

  import d3.syntax.svg._

  def run: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val genData = rng.nextDouble.map(_ * 2.0 * math.Pi).replicateA(3)
    val radius = 100.0

    val setup = for {
      _ <- d3
        .select("#app")
        .append("svg")
        .attr("width", "800".some)
        .attr("height", "300".some)
        .compile
        .drain
      _ <- d3
        .select("svg")
        .append("circle")
        .attr("style", "fill: none; stroke: #ccc; stroke-dasharray: 1,1".some)
        .attr("cx", "150".some)
        .attr("cy", "150".some)
        .attr("r", s"$radius".some)
        .compile
        .drain
      _ <- d3
        .select("svg")
        .append("g")
        .attr("transform", "translate(150, 150)".some)
        .compile
        .drain
    } yield ()

    val loop = Stream
      .fixedDelay(1.second)
      .evalMap(_ => genData)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("svg g")
          .selectAll[dom.Element, Double]("circle")
          .data(data)
          .join[F, dom.Element, Double, dom.Element, Nothing](
            _.append[dom.Element]("circle")
              .attr(r, "7".some)
              .attr(fill, "blue".some)
              .attr(cx, "0".some)
              .attr(cy, "0".some)
          )
          .transition
          .attr(cx)((_, d, _, _) => s"${radius * math.cos(d)}".some)
          .attr(cy)((_, d, _, _) => s"${radius * math.sin(d)}".some)
          .compile
          .drain

      }
      .compile
      .drain

    setup >> loop
  }

}
