package d3.selection.examples

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

  val f1 = F.delay {

    val elm = dom.document.querySelector("#app")
    elm.setAttribute("foo", "bar")
    // elm.append(dom.document.createElement("span"))

  }

  case class Foo(s: String, i: Int)

  def run: F[Unit] = {

    val data = List(Foo("foo", 1), Foo("bar", 2), Foo("qux", 3), Foo("baz", 4))

    val sel1 =
      Selection
        .select[F, dom.HTMLDivElement, Unit]("#app")
        .append("span")
        .append("p")
        .attr("foo", "bar")
        .text("Hello, World!")

//d3.select("#app")
//  .append("div")
//  .selectAll("span")
//  .data([1, 2, 3, 4, 5, 6])
//  .join(
//    enter => enter.append("span").text(i => `foo: ${i}`),
//    update => update,
//    exit => exit.remove()
//  )
//
//

    Random.scalaUtilRandom[F].flatMap { rng =>
      val randomData2 = rng
        .nextIntBounded(5)
        .flatMap { i =>
          rng.nextIntBounded(10).replicateA(i)
        }
        .map(_.map { i => Foo(s"foo-$i", i) })

      val randomData = rng.nextInt.replicateA(3).map { ints =>
        ints.map { i => Foo(s"foo-$i", i) }
      }

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
                    .text((n, d, i, g) => s"foo: $d"),
                update => update.classed("text-pink-500", true)
              )

          sel2.compile
        }
        .compile
        .drain
    }

  }

}
