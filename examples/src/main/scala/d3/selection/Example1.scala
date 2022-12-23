package d3.selection.examples

import cats.effect.implicits._
import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example1[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = (run1, run2).parTupled.void

  def run1: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val letters = ('a' to 'z').toList.map(_.toString)

    val randomLetters = rng.betweenInt(6, 26).flatMap { n =>
      rng.shuffleList(letters).map(_.take(n))
    }

    val width = "400"
    val height = "50"

    val setup = d3
      .select("#app")
      .append("svg")
      .attr("id", "demo1")
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", s"0 0 $width $height")
      .compile
      .drain

    val transDuration = 750.millis

    val loop = (Stream.emit(()) ++ Stream
      .fixedDelay[F](1.second))
      .evalMap(_ => randomLetters)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("#demo1")
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

  def run2: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val genData = rng.nextDouble.map(_ * 2.0 * math.Pi).replicateA(3)
    val radius = 100.0
    val transitionDuration = 750.millis

    val setup = for {
      _ <- d3
        .select("#app")
        .append("svg")
        .attr("id", "demo2")
        .attr("width", "300")
        .attr("height", "300")
        .compile
        .drain
      _ <- d3
        .select("#demo2")
        .append("circle")
        .attr("style", "fill: none; stroke: #ccc; stroke-dasharray: 1,1")
        .attr("cx", "150")
        .attr("cy", "150")
        .attr("r", s"$radius")
        .compile
        .drain
      _ <- d3
        .select("#demo2")
        .append("g")
        .attr("transform", "translate(150, 150)")
        .compile
        .drain
    } yield ()

    val loop = Stream
      .fixedDelay(1.second)
      .evalMap(_ => genData)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("#demo2 g")
          .selectAll[dom.Element, Double]("circle")
          .data(data)
          .join(
            _.append[dom.Element]("circle")
              .attr("r", "7")
              .attr("fill", "blue")
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

    setup >> loop
  }

}
