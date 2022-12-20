package d3.selection.examples

import cats.effect.kernel.Async

class Example3[F[_]](implicit F: Async[F]) {

  def run: F[Unit] =
    d3.select("#app")
      .append("span")
      .append("p")
      .attr("foo", "bar")
      .text("Hello, World!")
      .compile

}
