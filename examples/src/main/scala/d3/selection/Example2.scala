package d3.selection.examples

import cats.effect.kernel.Async
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom

import concurrent.duration._

class Example2[F[_]](implicit F: Async[F]) {

  case class Foo(s: String, i: Int)

  def run: F[Unit] = {

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
            d3.select[F, dom.Element, Nothing]("#app")
              .selectAll[dom.Element, Foo]("span")
              .keyedData(
                data
              )(
                (_, d, _, _) => d.toString,
                (_, d, _, _) => d.toString
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
