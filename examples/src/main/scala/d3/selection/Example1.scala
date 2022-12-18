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

  def run: F[Unit] = run2

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

}
