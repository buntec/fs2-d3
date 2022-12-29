package d3.scale

import cats.implicits._

import scala.collection.Searching.Found
import scala.collection.Searching.InsertionPoint
import d3.interpolate.Interpolator

trait ScaleContinuous[A] {

  def apply(x: Double): A

}

trait ScaleContinuousNumeric extends ScaleContinuous[Double] {

  def invert(x: Double): Double

}

object ScaleContinuous {

  def genericNumeric(
      domain: (Double, Double),
      range: (Double, Double),
      clamp: Boolean,
      transform: Double => Double,
      untransform: Double => Double
  ): ScaleContinuousNumeric = {

    val d0 = domain._1
    val d1 = domain._2
    val m = bimap((transform(d0), transform(d1)), range)
    val c = if (clamp) clamper(domain._1, domain._2) else identity

    val mi = bimap(range, (transform(d0), transform(d1)))

    new ScaleContinuousNumeric {
      override def apply(x: Double): Double = m(transform(c(x)))
      override def invert(x: Double): Double = c(untransform(mi(x)))
    }

  }

  def generic[A: Interpolator](
      domain: (Double, Double),
      range: (A, A),
      clamp: Boolean,
      transform: Double => Double
  ): ScaleContinuous[A] = {
    val d0 = domain._1
    val d1 = domain._2
    val m = bimap((transform(d0), transform(d1)), range)
    val c = if (clamp) clamper(domain._1, domain._2) else identity
    new ScaleContinuous[A] {
      override def apply(x: Double): A = m(transform(c(x)))
    }
  }

  def generic[V: Interpolator](
      domain: List[Double],
      range: List[V],
      clamp: Boolean,
      transform: Double => Double
  ): ScaleContinuous[V] = {
    val transformedDomain = domain.map(transform)
    val m = polymap(transformedDomain, range)
    val c = if (clamp) clamper(domain.min, domain.max) else identity
    new ScaleContinuous[V] {
      override def apply(x: Double): V = m(transform(c(x)))
    }
  }

  private def normalize(a: Double, b: Double): Double => Double = {
    require(a < b)
    (x: Double) => (x - a) / (b - a)
  }

  private def clamper(a: Double, b: Double): Double => Double = {
    require(a < b)
    (x: Double) => math.max(a, math.min(b, x))
  }

  private val identity = (x: Double) => x

  private def bimap[V: Interpolator](
      domain: (Double, Double),
      range: (V, V)
  ): Double => V = {
    val interp = Interpolator[V]
    val d0 = domain._1
    val d1 = domain._2
    val r0 = range._1
    val r1 = range._2
    if (d1 < d0) {
      bimap((d1, d0), (r1, r0))
    } else {
      val n = normalize(d0, d1)
      val i = interp(r0, r1)
      (x: Double) => i(n(x))
    }
  }

  private def polymap[V: Interpolator](
      domain: List[Double],
      range: List[V]
  ): Double => V = {

    val interp = Interpolator[V]
    val domainPairs = domain.zip(domain.tail)
    val rangePairs = range.zip(range.tail)

    val is = domainPairs
      .zip(rangePairs)
      .map { case ((d0, d1), (r0, r1)) =>
        val n = normalize(d0, d1)
        val i = interp(r0, r1)
        (x: Double) => i(n(x))
      }
      .toVector

    val domainV = domain.toVector

    (x: Double) =>
      domainV.search(x, 1, is.length) match {
        case Found(foundIndex)              => is(foundIndex - 1)(x)
        case InsertionPoint(insertionPoint) => is(insertionPoint - 1)(x)
      }

  }

}
