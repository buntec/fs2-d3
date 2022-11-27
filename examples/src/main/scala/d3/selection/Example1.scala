package d3.selection.examples

import cats.effect.kernel.Async

import scalajs.js

import org.scalajs.dom
import d3.selection.Selection

class Example1[F[_]](implicit F: Async[F]) {

  val f1 = F.delay {

    val elm = dom.document.querySelector("#app")
    elm.setAttribute("foo", "bar")
    // elm.append(dom.document.createElement("span"))

  }

  def run: F[Unit] = {

    val sel =
      Selection
        .select[F, dom.HTMLDivElement, Unit]("#app")
        .append("span")
        .append("p")
        .attr("foo", "bar")
        .text("Hello, World!")

    sel.compile

  }

}
