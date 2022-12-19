package d3.selection.examples

import cats.effect.kernel.Async
import d3.selection._
import org.scalajs.dom

class Example3[F[_]](implicit F: Async[F]) {

  def run: F[Unit] = {
    select[F, dom.HTMLDivElement, Unit]("#app")
      .append("span")
      .append("p")
      .attr("foo", "bar")
      .text("Hello, World!")
      .compile
  }
}
