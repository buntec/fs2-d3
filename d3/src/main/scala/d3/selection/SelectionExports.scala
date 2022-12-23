package d3.selection

import org.scalajs.dom

trait SelectionExports {

  def select[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] = Selection.select(selector)

  def select[F[_], N, D](node: N): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.Terminal(List(List(node)), List(null))

  def selectAll[F[_], N, D](
      selector: String
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.selectAll(selector)

  def selectAll[F[_], N, D](
      nodes: List[N]
  ): Selection[F, N, D, dom.HTMLElement, Unit] =
    Selection.Terminal(List(nodes), List(null))

}
