package d3.interpolate

object number {

  def apply(a: Double, b: Double): Double => Double = {
    (t: Double) => (1 - t) * a + t * b
  }

}
