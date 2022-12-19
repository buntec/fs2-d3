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

  case class Foo(s: String, i: Int)

  def run: F[Unit] = run3

  def run1: F[Unit] = {

    select[F, dom.HTMLDivElement, Unit]("#app")
      .append("span")
      .append("p")
      .attr("foo", "bar")
      .text("Hello, World!")
      .compile
  }

  def run2: F[Unit] = {

    Random.scalaUtilRandom[F].flatMap { rng =>
      val randomData2 = rng
        .nextIntBounded(5)
        .flatMap { i =>
          rng.nextIntBounded(10).replicateA(i)
        }
        .map(_.map { i => Foo(s"foo-$i", i) })

      Stream
        .fixedDelay(1.second)
        .evalMap(_ => randomData2)
        .evalMap { data =>
          val sel2 =
            Selection
              .select[F, dom.HTMLDivElement, Unit]("#app")
              .selectAll[dom.Element, Foo]("span")
              .data[Foo](
                data,
                (_: Any, d: Foo, _: Any, _: Any) => d.toString,
                (_: Any, d: Foo, _: Any, _: Any) => d.toString
              )
              .join[F, dom.Element, dom.Element, Foo, dom.Element, Unit](
                enter =>
                  enter
                    .append[dom.Element]("span")
                    .text((_, d, _, _) => s"foo: $d"),
                update => update.classed("text-pink-500", true)
              )

          sel2.compile
        }
        .compile
        .drain
    }

  }

  def run3: F[Unit] = {

    val letters = List(
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

      val setup = select[F, dom.HTMLDivElement, Unit]("#app")
        .append("svg")
        .compile

      val loop = Stream
        .fixedDelay[F](3.second)
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
                  .text((_, d, _, _) => d),
              update => update.attr("fill", "black")
            )
            .compile
        }
        .compile
        .drain

      setup >> loop

    }

  }

}
