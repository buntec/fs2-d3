package d3.selection.examples

import cats.effect.kernel.Async
import org.scalajs.dom

class Example3[F[_]](implicit F: Async[F]) {

  val data = List("foo", "bar", "baz")

  def run: F[Unit] =
    d3.select[F, dom.Element, Nothing]("#app")
      .append("span")
      .selectAll[dom.Element, String]("span")
      .data(data)
      .join(_.append("p"))
      .append[F, dom.Element] { (_, d, _, _) =>
        F.delay {
          val node = dom.document.createElement("div")
          node.textContent = d
          node
        }
      }
      .property("fooProp", (_, d, _, _) => Some(d))
      .compile
      .drain

}
