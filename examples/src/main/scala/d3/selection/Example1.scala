package d3.selection.examples

import d3.selection._

import cats.effect.kernel.Async

import cats.effect.implicits._
import cats.syntax.all._
import scalajs.js

import org.scalajs.dom
import d3.selection.Selection
import cats.effect.std.Random
import fs2.Stream
import concurrent.duration._

class Example1[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = {

    // const svg = d3.create("svg")
    //     .attr("width", width)
    //     .attr("height", 33)
    //     .attr("viewBox", `0 -20 ${width} 33`);

    // while (true) {
    //   const t = svg.transition()
    //       .duration(750);

    //   svg.selectAll("text")
    //     .data(randomLetters(), d => d)
    //     .join(
    //       enter => enter.append("text")
    //           .attr("fill", "green")
    //           .attr("x", (d, i) => i * 16)
    //           .attr("y", -30)
    //           .text(d => d)
    //         .call(enter => enter.transition(t)
    //           .attr("y", 0)),
    //       update => update
    //           .attr("fill", "black")
    //           .attr("y", 0)
    //         .call(update => update.transition(t)
    //           .attr("x", (d, i) => i * 16)),
    //       exit => exit
    //           .attr("fill", "brown")
    //         .call(exit => exit.transition(t)
    //           .attr("y", 30)
    //           .remove())
    //     );

    Random.scalaUtilRandom[F].flatMap { rng =>
      val randomLetters = rng.betweenInt(6, 26).flatMap { n =>
        rng.shuffleList(letters).map(_.take(n))
      }

      val width = "1000"
      val height = "50"

      val setup = select[F, dom.HTMLDivElement, Unit]("#app")
        .append("svg")
        .attr("width", width)
        .attr("height", height)
        .attr("viewBox", s"0 0 $width $height")
        .compile

      val loop = (Stream.emit(()) ++ Stream
        .fixedDelay[F](3.second))
        .evalMap(_ => randomLetters)
        .evalMap { data =>
          Selection
            .select[F, dom.HTMLDivElement, Unit]("#app")
            .select("svg")
            .selectAll[dom.Element, String]("text")
            .data[String](
              data,
              (_: Any, d: String, _: Any, _: Any) => d,
              (_: Any, d: String, _: Any, _: Any) => d
            )
            .join[F, dom.Element, dom.Element, String, dom.Element, Unit](
              enter =>
                enter
                  .append[dom.Element]("text")
                  .attr("fill", "green")
                  .attr("x", (_, _, i, _) => s"${16 * i}")
                  .attr("y", "0")
                  .attrTransition("y", "25", 1.second, 0.seconds)
                  .text((_, d, _, _) => d),
              update =>
                update
                  .attr("fill", "black")
                  .attrTransition(
                    "x",
                    (_, _, i, _) => s"${16 * i}",
                    1.second,
                    0.seconds
                  ),
              exit =>
                exit
                  .attrTransition("fill", "brown", 1.second, 0.seconds)
                  .attrTransition("y", "50", 1.second, 0.seconds)
                  .remove
            )
            .compile
        }
        .compile
        .drain

      setup >> loop

    }

  }

  private val letters = List(
    "a",
    "b",
    "c",
    "d",
    "e",
    "f",
    "g",
    "h",
    "i",
    "j",
    "k",
    "l",
    "m",
    "n",
    "o",
    "p",
    "q",
    "r",
    "s",
    "t",
    "u",
    "v",
    "w",
    "x",
    "y",
    "z"
  )

}
