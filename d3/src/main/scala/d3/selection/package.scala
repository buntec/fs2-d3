package d3

import org.scalajs.dom

package object selection {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] = Selection.select(selector)

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] = Selection.selectAll(selector)

}
