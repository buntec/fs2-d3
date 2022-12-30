package d3

import cats.effect.kernel.Async
import cats.syntax.all._
import d3.scale.ScaleContinuousNumeric
import d3.selection.Selection
import org.scalajs.dom

package object axis {

  def axisTop[F[_], N, D, PN, PD](
      scale: ScaleContinuousNumeric
  )(implicit F: Async[F]): Selection[F, N, D, PN, PD] => F[Unit] =
    axis(Orientation.Top(), scale)

  def axisRight[F[_], N, D, PN, PD](
      scale: ScaleContinuousNumeric
  )(implicit F: Async[F]): Selection[F, N, D, PN, PD] => F[Unit] =
    axis(Orientation.Right(), scale)

  def axisBottom[F[_], N, D, PN, PD](
      scale: ScaleContinuousNumeric
  )(implicit F: Async[F]): Selection[F, N, D, PN, PD] => F[Unit] =
    axis(Orientation.Bottom(), scale)

  def axisLeft[F[_], N, D, PN, PD](
      scale: ScaleContinuousNumeric
  )(implicit F: Async[F]): Selection[F, N, D, PN, PD] => F[Unit] =
    axis(Orientation.Left(), scale)

  def axis[F[_], N, D, PN, PD](
      orient: Orientation,
      scale: ScaleContinuousNumeric
  )(implicit F: Async[F]): Selection[F, N, D, PN, PD] => F[Unit] = {
    val tickSizeOuter = 6
    val tickSizeInner = 6
    val tickPadding = 3
    val offset = 0.0
    val values = scale.ticks
    val format = scale.tickFormat
    val k = orient match {
      case Orientation.Bottom() => 1
      case Orientation.Left()   => -1
      case Orientation.Right()  => 1
      case Orientation.Top()    => -1
    }
    val x = orient match {
      case Orientation.Bottom() => "y"
      case Orientation.Left()   => "x"
      case Orientation.Right()  => "x"
      case Orientation.Top()    => "y"
    }
    val transform = orient match {
      case Orientation.Bottom() => translateX _
      case Orientation.Left()   => translateY _
      case Orientation.Right()  => translateY _
      case Orientation.Top()    => translateX _
    }
    val position = scale.apply _

    sel => {

      val range0 = scale.rangeMin + offset
      val range1 = scale.rangeMax + offset
      val spacing = math.max(tickSizeInner, 0) + tickPadding

      sel
        .append("g")
        .attr("fill", Some("none"))
        .attr("font-size", Some("10"))
        .attr("font-family", Some("sans-serif"))
        .attr(
          "text-anchor",
          Some(
            orient match {
              case Orientation.Bottom() => "middle"
              case Orientation.Left()   => "end"
              case Orientation.Right()  => "start"
              case Orientation.Top()    => "middle"
            }
          )
        )
        .compile
        .nodeOrError[F, dom.Element]
        .flatMap { root =>
          val path = d3
            .select[F, dom.Element, Nothing](root)
            .selectAll[dom.Element, Nothing](".domain")
            .data(List(()))
            .join[F, dom.Element, Unit, Any, Any](_.append("path"))
            .attr("class", Some("domain"))
            .attr("stroke", Some("currentColor"))
            .attr(
              "d",
              orient match {
                case Orientation.Left() | Orientation.Right() =>
                  Some(
                    s"M${k * tickSizeOuter},${range0}H${offset}V${range1}H${k * tickSizeOuter}"
                  )
                case _ =>
                  Some(
                    s"M${range0},${k * tickSizeOuter}V${offset}H${range1}V${k * tickSizeOuter}"
                  )
              }
            )

          val tick = d3
            .select[F, dom.Element, Nothing](root)
            .selectAll[dom.Element, Double](".tick")
            .dataKeyed(values)(
              (_, d, _, _) => scale(d).toString,
              (_, d, _, _) => scale(d).toString
            )
            .join[F, dom.Element, Double, Any, Any](
              _.append("g")
                .attr("class", Some("tick"))
                .append("line")
                .attr("stroke", Some("currentColor"))
                .selectParent
                .append("text")
                .attr("fill", Some("currentColor"))
                .selectParent
            )
            .attr("opacity", Some("1"))
            .attr("transform")((_, d, _, _) =>
              Some(transform(position(d) + offset))
            )
            .select[dom.Element]("line")
            .attr("stroke", Some("currentColor"))
            .attr(s"${x}2", Some(s"${k * tickSizeInner}"))
            .selectParent[dom.Element]
            .select[dom.Element]("text")
            .attr(x, Some(s"${k * spacing}"))
            .attr(
              "dy",
              Some(
                orient match {
                  case Orientation.Bottom() => "0.71em"
                  case Orientation.Left()   => "0.32em"
                  case Orientation.Right()  => "0.32em"
                  case Orientation.Top()    => "0em"
                }
              )
            )
            .text((_, d, _, _) => format(d))
            .selectParent

          path.compile.drain >> tick.compile.drain
        }
    }
  }

  private def translateX(x: Double): String = s"translate($x,0)"

  private def translateY(y: Double): String = s"translate(0,$y)"

}
