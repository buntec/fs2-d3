package d3.selection.examples

import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example1[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = Random.scalaUtilRandom[F].flatMap { rng =>
    val letters = ('a' to 'z').toList.map(_.toString)

    val randomLetters = rng.betweenInt(6, 26).flatMap { n =>
      rng.shuffleList(letters).map(_.take(n))
    }

    val width = "1000"
    val height = "50"

    val setup = d3
      .select[F, dom.Element, Unit]("#app")
      .append("svg")
      .attr("width", width)
      .attr("height", height)
      .attr("viewBox", s"0 0 $width $height")
      .compile

    val transDuration = 750.millis

    val loop = (Stream.emit(()) ++ Stream
      .fixedDelay[F](1.second))
      .evalMap(_ => randomLetters)
      .evalMap { data =>
        d3.select[F, dom.Element, Nothing]("#app")
          .select[dom.Element]("svg")
          .selectAll[dom.Element, String]("text")
          .keyedData(data)(
            (_, d, _, _) => d,
            (_, d, _, _) => d
          )
          .join(
            enter =>
              enter
                .append[dom.Element]("text")
                .attr("fill", "green")
                .attr("opacity", "1.0")
                .attr("x", (_, _, i, _) => s"${16 * i}")
                .attr("y", "0")
                .attrTransition("y", "25", transDuration, 0.seconds)
                .text((_, d, _, _) => d),
            update =>
              update
                .attr("fill", "black")
                .attrTransition(
                  "x",
                  (_, _, i, _) => s"${16 * i}",
                  transDuration,
                  0.seconds
                ),
            exit =>
              exit
                .attr("fill", "brown")
                .attrTransition("y", "50", transDuration, 0.seconds)
                .attrTransition("opacity", "0", transDuration, 0.seconds)
                .removeAfterTransition
          )
          .compile
      }
      .compile
      .drain

    setup >> loop

  }

}
