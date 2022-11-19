package d3.selection

import org.scalajs.dom
import scalajs.js
import scala.collection.mutable.ArrayBuffer

import Selection._

sealed abstract class Selection[F[_], N, D] {

  def select[N0](selector: String): Selection[F, N0, D] = new Bind[F, N, N0, D, D](this) {
  }

  def selectAll[N0](selector: String): Selection[F, N0, D] = ???

  def append[N0](tpe: String): Selection[F, N0, D] = ???

  def data[D0](value: List[D0], key: D0 => String): Selection[F, N, D0] = ???

}

object Selection {

  def select[F[_], N](selector: String): Selection[Nothing, N, Nothing] =
    SingleElement[N](selector)

  private abstract class Bind[F[_], N1, N2, D1, D2](
      val step: Selection[F, N1, D1]
  ) extends Selection[F, N2, D2] {
    def cont(r: Terminal[N2]): Selection[F, N2, D2]
  }

  private sealed abstract class Terminal[N]
      extends Selection[Nothing, N, Nothing]

  private case class SingleElement[N](selector: String) extends Terminal[N]

}
