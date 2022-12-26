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

package d3.selection.examples

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std.Dispatcher
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example1[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = globalSetup >> (demo1, demo2, demo3).parTupled.void

  val globalSetup =
    d3.select("#app")
      .classed("flex flex-col items-center mx-8", true)
      .selectAll("div")
      .data(List(1, 2, 3))
      .join[F, dom.Element, Int, dom.Element, Nothing](
        _.append[dom.Element]("div")
          .classed(
            "p-4 flex flex-col items-center text-lg w-full border-b",
            true
          )
          .attr("id", (_, _, i, _) => s"demo-${i + 1}".some)
      )
      .compile
      .drain

  def demo1: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val letters = ('a' to 'z').toList.map(_.toString)
    val randomLetters = rng.betweenInt(6, 26).flatMap { n =>
      rng.shuffleList(letters).map(_.take(n))
    }

    val width = "400"
    val height = "50"

    val setup = for {
      _ <- d3
        .select("#demo-1")
        .append("h1")
        .text("An example using selection + transition")
        .compile
        .drain
      svg <- d3
        .select("#demo-1")
        .append("svg")
        .attr("width", width.some)
        .attr("height", height.some)
        .attr("viewBox", s"0 0 $width $height".some)
        .compile
        .nodeOrError[F, dom.Element]
    } yield svg

    setup.flatMap { svg =>
      (Stream.emit(()) ++ Stream
        .fixedDelay[F](1.second))
        .evalMap(_ => randomLetters)
        .evalMap { data =>
          d3.select[F, dom.Element, Nothing](svg)
            .selectAll[dom.Element, String]("text")
            .dataKeyed(data)(
              (_, d, _, _) => d,
              (_, d, _, _) => d
            )
            .join[F, dom.Element, String, dom.Element, Nothing](
              // enter
              _.append[dom.Element]("text")
                .attr("fill", "green".some)
                .attr("opacity", "1.0".some)
                .attr("x", (_, _, i, _) => s"${16 * i}".some)
                .attr("y", "0".some)
                .text((_, d, _, _) => d)
                .transition
                .duration(500.millis)
                .ease(d3.ease.easeBounce)
                .attr("y", "25")
                .selection,
              // update
              _.attr("fill", "black".some).transition
                .attr(
                  "x",
                  (_, _, i, _) => s"${16 * i}"
                )
                .selection,
              // exit
              _.attr("fill", "brown".some).transition
                .attr("y", "50")
                .attr("opacity", "0")
                .remove
                .selection
            )
            .compile
            .drain
        }
        .compile
        .drain
    }

  }

  def demo2: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val genData = rng.nextDouble.replicateA(8)
    val radius = 100.0
    val colors = d3.color.named.keySet.toVector
    val nColors = colors.length

    val setup = for {
      _ <- d3
        .select("#demo-2")
        .append("h1")
        .text("An example using selection + transition")
        .compile
        .drain
      root <- d3
        .select("#demo-2")
        .append("svg")
        .attr("width", "300".some)
        .attr("height", "300".some)
        .compile
        .nodeOrError[F, dom.Element]
      _ <- d3
        .select[F, dom.Element, Nothing](root)
        .append("circle")
        .attr("style", "fill: none; stroke: #ccc; stroke-dasharray: 1,1".some)
        .attr("cx", "150".some)
        .attr("cy", "150".some)
        .attr("r", s"$radius".some)
        .compile
        .drain
      g <- d3
        .select[F, dom.Element, Nothing](root)
        .append("g")
        .attr("transform", "translate(150, 150)".some)
        .compile
        .nodeOrError[F, dom.Element]
    } yield g

    setup.flatMap { case root =>
      Stream
        .fixedDelay(1.second)
        .evalMap(_ => genData)
        .evalMap { data =>
          d3.select(root)
            .selectAll("circle")
            .data(data)
            .join[F, dom.Element, Double, dom.Element, Nothing](
              // enter
              _.append[dom.Element]("circle")
                .attr("r", "7".some)
                .attr("fill", "gray".some)
                .attr("cx", "0".some)
                .attr("cy", "0".some)
            )
            .transition
            .delay((_, d, _, _) => (d * 500.0).millis)
            .attr(
              "cx",
              (_, d, _, _) => s"${radius * math.cos(d * 2 * math.Pi)}"
            )
            .attr(
              "cy",
              (_, d, _, _) => s"${radius * math.sin(d * 2 * math.Pi)}"
            )
            .attr(
              "fill",
              (_, d, _, _) =>
                colors(math.min(math.round(d * nColors).toInt, nColors - 1))
            )
            .compile
            .drain

        }
        .compile
        .drain
    }

  }

  def demo3: F[Unit] = Dispatcher.parallel[F].use { dispatcher =>
    val width = "150"
    val height = "50"
    val data = List("1", "2", "3")

    val setup = for {
      _ <- d3
        .select("#demo-3")
        .append("h1")
        .text(
          "An example using event listeners - click on the circles to change their color"
        )
        .compile
        .drain
      svg <- d3
        .select("#demo-3")
        .append[dom.Element]("svg")
        .attr("id", "demo3".some)
        .attr("width", width.some)
        .attr("height", height.some)
        .attr("viewBox", s"0 0 $width $height".some)
        .compile
        .nodeOrError
    } yield svg

    setup.flatMap { svg =>
      d3.select(svg)
        .selectAll("circle")
        .data(data)
        .join[F, dom.Element, String, dom.Element, Nothing](
          // enter
          _.append[dom.Element]("circle")
            .attr("r", "10".some)
            .attr("fill", "gray".some)
            .attr("cx", (_, _, i, _) => s"${50 * i + 25}".some)
            .attr("cy", "25".some)
            .style("cursor", "pointer".some)
            .on(
              "click",
              Some((n: dom.Element, _: dom.Event, _: String) =>
                d3.select(n).compile.attr("fill").flatMap { fill =>
                  val currentColor = fill.flatMap { f =>
                    d3.color.fromString(f)
                  }
                  val color1 = d3.color.fromString("green").get
                  val color2 = d3.color.fromString("red").get
                  val newFill =
                    if (currentColor.exists(_ == color1)) color2 else color1
                  d3.select(n)
                    .transition
                    .attr("r", "15")
                    .attr("fill", newFill.toString)
                    .transition
                    .duration(250.millis)
                    .attr("r", "10")
                    .compile
                    .drain
                }
              ),
              None,
              dispatcher
            )
        )
        .compile
        .drain
    } >> F.never // we need `never` here to keep the dispatcher alive

  }

}
