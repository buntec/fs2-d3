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
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._
import cats.effect.std.Dispatcher

class Example1[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = globalSetup >> (demo1, demo2, demo3).parTupled.void

  val globalSetup =
    d3.select("#app")
      .classed("flex flex-col items-center", true)
      .selectAll("div")
      .data(List(1, 2, 3))
      .join(enter =>
        enter
          .append[dom.Element]("div")
          .classed("m-2 p-2 flex flex-col items-center text-lg", true)
          .attr("id", (_, _, i, _) => s"demo-${i + 1}")
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
      _ <- d3
        .select("#demo-1")
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", s"0 0 $width $height")
        .compile
        .drain
    } yield ()

    val transDuration = 750.millis

    val loop = (Stream.emit(()) ++ Stream
      .fixedDelay[F](1.second))
      .evalMap(_ => randomLetters)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("#demo-1 svg")
          .selectAll[dom.Element, String]("text")
          .dataKeyed(data)(
            (_, d, _, _) => d,
            (_, d, _, _) => d
          )
          .join(
            // enter
            _.append[dom.Element]("text")
              .attr("fill", "green")
              .attr("opacity", "1.0")
              .attr("x", (_, _, i, _) => s"${16 * i}")
              .attr("y", "0")
              .text((_, d, _, _) => d)
              .transition
              .attr("y", "25", transDuration, 0.seconds),
            // update
            _.attr("fill", "black").transition
              .attr(
                "x",
                (_, _, i, _) => s"${16 * i}",
                transDuration,
                0.seconds
              ),
            // exit
            _.attr("fill", "brown").transition
              .attr("y", "50", transDuration, 0.seconds)
              .attr("opacity", "0", transDuration, 0.seconds)
              .remove
          )
          .compile
          .drain
      }
      .compile
      .drain

    setup >> loop

  }

  def demo2: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val genData = rng.nextDouble.map(_ * 2.0 * math.Pi).replicateA(3)
    val radius = 100.0
    val transitionDuration = 750.millis

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
        .attr("width", "300")
        .attr("height", "300")
        .compile
        .node[F, dom.Element]
      _ <- d3
        .select[F, dom.Element, Nothing](root.get)
        .append("circle")
        .attr("style", "fill: none; stroke: #ccc; stroke-dasharray: 1,1")
        .attr("cx", "150")
        .attr("cy", "150")
        .attr("r", s"$radius")
        .compile
        .drain
      g <- d3
        .select[F, dom.Element, Nothing](root.get)
        .append("g")
        .attr("transform", "translate(150, 150)")
        .compile
        .node[F, dom.Element]
    } yield g.get

    setup.flatMap { case root =>
      Stream
        .fixedDelay(1.second)
        .evalMap(_ => genData)
        .evalMap { data =>
          d3.select(root)
            .selectAll[dom.Element, Double]("circle")
            .data(data)
            .join(
              _.append[dom.Element]("circle")
                .attr("r", "7")
                .attr("fill", "gray")
                .attr("cx", "0")
                .attr("cy", "0")
            )
            .transition
            .attr(
              "cx",
              (_, d, _, _) => s"${radius * math.cos(d)}",
              transitionDuration,
              0.seconds
            )
            .attr(
              "cy",
              (_, d, _, _) => s"${radius * math.sin(d)}",
              transitionDuration,
              0.seconds
            )
            .compile
            .drain

        }
        .compile
        .drain
    }

  }

  def demo3: F[Unit] = Dispatcher.sequential[F].use { dispatcher =>
    val width = "400"
    val height = "50"
    val data = List("1", "2", "3", "4", "5", "6", "7", "8")

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
        .attr("id", "demo3")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", s"0 0 $width $height")
        .compile
        .node
    } yield svg.get

    setup.flatMap { svg =>
      d3.select(svg)
        .selectAll("circle")
        .data(data)
        .join(
          _.append[dom.Element]("circle")
            .attr("r", "10")
            .attr("fill", "gray")
            .attr("cx", (_, _, i, _) => s"${50 * i + 10}")
            .attr("cy", "25")
            .on(
              "click",
              Some((n: dom.Element, _: dom.Event, _: String) =>
                d3.select(n).compile.style("fill").flatMap { fill =>
                  val newFill =
                    if (fill.exists(_ == "orange")) "gray" else "orange"
                  d3.select(n).style("fill", Some(newFill)).compile.drain
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
