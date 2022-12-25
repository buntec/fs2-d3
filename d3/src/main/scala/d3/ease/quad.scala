package d3.ease

object quad {

  def quadIn(t: Double): Double = t * t

  def quadOut(t: Double): Double = t * (2 - t)

  def quadInOut(t: Double): Double = {
    val t2 = 2.0 * t
    if (t2 <= 1) (t2 * t2) / 2.0
    else ((t2 - 1) * (3 - t2) + 1) / 2.0
  }

}
