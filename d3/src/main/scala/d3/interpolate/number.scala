package d3.interpolate

import cats.syntax.all._

object number {

  def apply(a: Double, b: Double): Double => Double = { (t: Double) =>
    (1 - t) * a + t * b
  }

  def apply(a: String, b: String): Option[Double => String] = {
    (a.toDoubleOption, b.toDoubleOption).tupled
      .map { case (a0, b0) =>
        val interp = apply(a0, b0)
        (t: Double) => interp(t).toString
      }
  }

}
