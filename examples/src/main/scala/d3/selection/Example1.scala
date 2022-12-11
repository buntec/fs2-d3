package d3.selection.examples

import cats.effect.kernel.Async

import cats.syntax.all._
import scalajs.js

import org.scalajs.dom
import d3.selection.Selection

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

    val sel2 =
      Selection
        .select[F, dom.HTMLDivElement, Unit]("#app")
        .append("div")
        .data(data)
        .join(enter => enter.append("span"), update => update.append("p"))

    sel2.compile

  }

}
