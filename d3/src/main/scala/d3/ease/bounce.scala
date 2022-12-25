package d3.ease

object bounce {

  private val b1 = 4.0 / 11
  private val b2 = 6.0 / 11
  private val b3 = 8.0 / 11
  private val b4 = 3.0 / 4
  private val b5 = 9.0 / 11
  private val b6 = 10.0 / 11
  private val b7 = 15.0 / 16
  private val b8 = 21.0 / 22
  private val b9 = 63.0 / 64
  private val b0 = 1.0 / b1 / b1

  def bounceOut(t: Double): Double = {
    if (t < b1) b0 * t * t
    else if (t < b3) b0 * (t - b2) * (t - b2) + b4
    else if (t < b6) b0 * (t - b5) * (t - b5) + b7
    else b0 * (t - b8) * (t - b8) + b9
  }

  def bounceIn(t: Double): Double = {
    1 - bounceOut(1 - t)
  }

  def bounceInOut(t: Double): Double = {
    val t2 = 2 * t
    if (t2 <= 1) (1 - bounceOut(1 - t2)) / 2
    else (bounceOut(t2 - 1) + 1) / 2
  }

}
