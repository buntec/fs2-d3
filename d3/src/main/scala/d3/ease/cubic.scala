package d3.ease

object cubic {

  def cubicIn(t: Double): Double = t * t * t

  def cubicOut(t: Double): Double =
    (t - 1) * (t - 1) * (t - 1) + 1

  def cubicInOut(t: Double): Double = {
    if (t < 0.5) {
      4 * t * t * t
    } else {
      1 - math.pow(-2 * t + 2, 3) / 2
    }
  }

}
