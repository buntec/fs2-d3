package d3.interpolate

object string {

  private val reA = """[-+]?(?:\d+.?\d*|.?\d+)(?:[eE][-+]?\d+)?""".r

  def apply(a: String, b: String): Double => String = {

    val ips = reA
      .findAllMatchIn(a)
      .zip(reA.findAllMatchIn(b))
      .map { case (ma, mb) =>
        mb.matched -> number.apply(ma.matched.toDouble, mb.matched.toDouble)
      }
      .toMap

    (t: Double) =>
      if (t <= 0) a
      else if (t >= 1) b
      else
        reA.replaceAllIn(
          b,
          m => ips.get(m.matched).fold(m.matched)(_(t).toString)
        )

  }

}
