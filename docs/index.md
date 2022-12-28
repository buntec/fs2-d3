# FS2-D3

**FS2-D3** is a pure [Scala.js](https://www.scala-js.org/) port of [D3.js](https://github.com/d3/d3).
It offers a purely-functional (i.e., referentially transparent) and type-safe API that should
feel familiar to users of D3.js.
The library is built on top of [Cats Effect](https://typelevel.org/cats-effect/) and [FS2](https://fs2.io/).
While the API is parametric in the effect type `F[_]`, in practice most users will want
to use the concrete type `cats.effect.IO`.

### Acknowledgements

Huge thanks to [@armanbilge](https://github.com/armanbilge/) for answering all of my stupid questions.

## Examples

In this first example, we generate once per second a random sequence of letters
and bind them to SVG text elements. We use transitions to animate the entering,
exiting and reordering of letters.

```scala mdoc:js
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom
import concurrent.duration._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

Random.scalaUtilRandom[IO].flatMap { rng =>
    val letters = ('a' to 'z').toList.map(_.toString)
    val randomLetters = rng.betweenInt(6, 26).flatMap { n =>
      rng.shuffleList(letters).map(_.take(n))
    }
    val w = "400"
    val h = "50"

    import d3.syntax.svg._

    val setup = d3
        .select(node)
        .append("svg")
        .attr(width, w.some)
        .attr(height, h.some)
        .attr(viewBox, s"0 0 $w $h".some)
        .compile
        .nodeOrError[IO, dom.Element]

    setup.flatMap { svg =>
      (Stream.emit(()) ++ Stream
        .fixedDelay[IO](1.second))
        .evalMap(_ => randomLetters)
        .evalMap { data =>
          d3.select[IO, dom.Element, Nothing](svg)
            .selectAll[dom.Element, String]("text")
            .dataKeyed(data)(
              (_, d, _, _) => d,
              (_, d, _, _) => d
            )
            .join[IO, dom.Element, String, dom.Element, Nothing](
              // enter
              _.append[dom.Element]("text")
                .attr(fill, "green".some)
                .attr(opacity, "1.0".some)
                .attr(x)((_, _, i, _) => s"${16 * i}".some)
                .attr(y, "0".some)
                .text((_, d, _, _) => d)
                .transition
                .duration(500.millis)
                .ease(d3.ease.easeBounce)
                .attr(y, "25".some)
                .selection,
              // update
              _.attr(fill, "black".some).transition
                .attr(x)((_, _, i, _) => s"${16 * i}".some)
                .selection,
              // exit
              _.attr(fill, "brown".some).transition
                .attr(y, "50".some)
                .attr(opacity, "0".some)
                .remove
                .selection
            )
            .compile
            .drain
        }
        .compile
        .drain
    }

  }.unsafeRunAndForget()

```

In the next example, we randomly place small colored circles on the boundary
of a larger circle. Again, we use transitions to animate the
changes in position and color.


```scala mdoc:js
import cats.effect.std.Random
import cats.syntax.all._
import fs2.Stream
import org.scalajs.dom
import concurrent.duration._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

Random.scalaUtilRandom[IO].flatMap { rng =>
    import d3.syntax.svg._

    val genData = rng.nextDouble.replicateA(8)
    val radius = 100.0
    val colors = d3.color.named.keySet.toVector
    val nColors = colors.length

    val setup = for {
      root <- d3
        .select(node)
        .append("svg")
        .attr(width, "300".some)
        .attr(height, "300".some)
        .compile
        .nodeOrError[IO, dom.Element]
      _ <- d3
        .select(root)
        .append("circle")
        .attr(style, "fill: none; stroke: #ccc; stroke-dasharray: 1,1".some)
        .attr(cx, "150".some)
        .attr(cy, "150".some)
        .attr(r, s"$radius".some)
        .compile
        .drain[IO]
      g <- d3
        .select(root)
        .append("g")
        .attr(transform, "translate(150, 150)".some)
        .compile
        .nodeOrError[IO, dom.Element]
    } yield g

    setup.flatMap { case root =>
      Stream
        .fixedDelay[IO](1.second)
        .evalMap(_ => genData)
        .evalMap { data =>
          d3.select(root)
            .selectAll("circle")
            .data(data)
            .join[IO, dom.Element, Double, dom.Element, Nothing](
              // enter
              _.append("circle")
                .attr(r, "7".some)
                .attr(fill, "gray".some)
                .attr(cx, "0".some)
                .attr(cy, "0".some)
            )
            .transition
            .delay((_, d, _, _) => (d * 500.0).millis)
            .attr(cx)((_, d, _, _) =>
              s"${radius * math.cos(d * 2 * math.Pi)}".some
            )
            .attr(cy)((_, d, _, _) =>
              s"${radius * math.sin(d * 2 * math.Pi)}".some
            )
            .attr(fill)((_, d, _, _) =>
              colors(math.min(math.round(d * nColors).toInt, nColors - 1)).some
            )
            .compile
            .drain

        }
        .compile
        .drain
    }

  }.unsafeRunAndForget()

```

In this example, we add interactivity by registering event listeners
for the "click" event. Clicking on any of the circles will change their color.

```scala mdoc:js
import cats.effect.std.Dispatcher
import cats.syntax.all._
import org.scalajs.dom
import concurrent.duration._
import cats.effect.IO
import cats.effect.unsafe.implicits.global

Dispatcher.parallel[IO].use { dispatcher =>
    val width = "150"
    val height = "50"
    val data = List("1", "2", "3")

    val setup = d3
        .select(node)
        .append[dom.Element]("svg")
        .attr("id", "demo3".some)
        .attr("width", width.some)
        .attr("height", height.some)
        .attr("viewBox", s"0 0 $width $height".some)
        .compile
        .nodeOrError[IO, dom.Element]

    import d3.syntax.svg._
    import d3.syntax.html._

    setup.flatMap { svg =>
      d3.select(svg)
        .selectAll("circle")
        .data(data)
        .join[IO, dom.Element, String, dom.Element, Nothing](
          // enter
          _.append[dom.Element]("circle")
            .attr(r, "10".some)
            .attr(fill, "gray".some)
            .attr(cx)((_, _, i, _) => s"${50 * i + 25}".some)
            .attr(cy, "25".some)
            .style(cursor.pointer)
            .on[IO](
              "click",
              Some((n: dom.Element, _: dom.Event, _: String) =>
                d3.select(n).compile.attr[IO]("fill").flatMap { fill0 =>
                  val currentColor = fill0.flatMap { f =>
                    d3.color.fromString(f)
                  }
                  val color1 = d3.color.fromString("green").get
                  val color2 = d3.color.fromString("red").get
                  val newFill =
                    if (currentColor.exists(_ == color1)) color2 else color1
                  d3.select(n)
                    .transition
                    .attr(r, "15".some)
                    .attr(fill, newFill.toString.some)
                    .transition
                    .duration(250.millis)
                    .attr(r, "10".some)
                    .compile
                    .drain[IO]
                }
              ),
              None,
              dispatcher
            )
        )
        .compile
        .drain
    } >> IO.never // we need `never` here to keep the dispatcher alive
  }.unsafeRunAndForget()

```
