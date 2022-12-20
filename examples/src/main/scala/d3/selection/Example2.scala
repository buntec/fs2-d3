package d3.selection.examples

import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example2[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val genData = rng.nextDouble.map(_ * 2.0 * math.Pi).replicateA(3)
    val radius = 100.0
    val transitionDuration = 750.millis

    val setup = for {
      _ <- d3
        .select("#app")
        .append("svg")
        .attr("width", "800")
        .attr("height", "300")
        .compile
      _ <- d3
        .select("svg")
        .append("circle")
        .attr("style", "fill: none; stroke: #ccc; stroke-dasharray: 1,1")
        .attr("cx", "150")
        .attr("cy", "150")
        .attr("r", s"$radius")
        .compile
      _ <- d3
        .select("svg")
        .append("g")
        .attr("transform", "translate(150, 150)")
        .compile
    } yield ()

    val loop = Stream
      .fixedDelay(1.second)
      .evalMap(_ => genData)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("svg g")
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

      }
      .compile
      .drain

    setup >> loop
  }

}
