package d3

import org.scalajs.dom

package object selection {

  def select(selector: String) = Selection(
    Seq(Seq(dom.document.querySelector(selector))),
    Seq(dom.document.documentElement)
  )

  def select(elm: dom.Element) = Selection(Seq(Seq(elm)), Seq(null))

  def selectAll(selector: String) = Selection(
    Seq(dom.document.querySelectorAll(selector).toSeq),
    Seq(dom.document.documentElement)
  )

  def selectAll(elms: Seq[dom.Element]) = Selection(Seq(elms), Seq(null))

}
