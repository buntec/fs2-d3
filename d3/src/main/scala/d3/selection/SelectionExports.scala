package d3.selection

import org.scalajs.dom

trait SelectionExports {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] = Selection.select(selector)

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.selectAll(selector)

}
