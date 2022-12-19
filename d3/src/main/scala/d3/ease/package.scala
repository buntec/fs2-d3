package d3

package object ease {

  def cubicInOut(t: Double): Double = {
    if (t < 0.5) {
      4 * t * t * t
    } else {
      1 - math.pow(-2 * t + 2, 3) / 2
    }
  }

}
